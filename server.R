function(input, output) {

  regime_filt <- reactive({
    gsub("U", NA, input$regime_filt)
    })

  plates2plot <-  reactive({
    plates[[input$plate_boundary_choice]]
  })

  model <- reactive({
    cpm_models |>
    filter(
      model == input$motion_choice
    )
  })

  por <-  reactive({
    if (input$plate_fix != "Enter plate..." & input$plate_rot != "Enter plate...") {
      equivalent_rotation(model(), input$plate_fix, input$plate_rot)
    } else {
      NULL
    }
  })



  stress_df_filt <-  reactive({
    stress_df |>
    filter(
      between(unc, input$quality_filt[1], input$quality_filt[2]),
      between(depth, input$depth_filt[1], input$depth_filt[2]),
      between(lat, input$lat_filt[1], input$lat_filt[2]),
      between(lon, input$lon_filt[1], input$lon_filt[2]),
      regime %in% regime_filt()
    )
  })

  trajectories <-  reactive({
  if (!is.null(por())) {
    if ("sc" %in% input$traj_filt) {
      sc <- eulerpole_smallcircles(por(), n = 36) |> select() |> mutate(type = "sc")
    } else {
      sc <- NULL
    }
    if ("gc" %in% input$traj_filt) {
      gc <- eulerpole_greatcircles(por(), n = 36)|> select() |> mutate(type = "gc")
    } else {
      gc <- NULL
    }
    if ("lc" %in% input$traj_filt) {
      lc <- eulerpole_loxodromes(por(), cw = TRUE, n = 36)|> select() |> mutate(type = "lc")
    } else {
      lc <- NULL
    }
    if ("lcc" %in% input$traj_filt) {
      lcc <- eulerpole_loxodromes(por(), cw = FALSE, n = 36) |> select() |> mutate(type = "lcc")
    } else {
      lcc <- NULL
    }
  } else {
    sc <- gc <- lc <- lcc <- land[0] |> sf::st_as_sf()|> select() |> mutate(type = "lcc")
  }
  dplyr::bind_rows(sc, gc, lc, lcc)
  })

  # pb_distance <- reactive({
  #   if (!is.null(por())) {
  #     pair = ifelse(input$plate_fix<input$plate_rot, paste0(input$plate_fix, "-", input$plate_rot), paste0(input$plate_rot, "-", input$plate_fix))
  #     plate2dist <- filter(plates2plot(), pair == pair)
  #
  #     t=TRUE
  #     if(input$prd_type %in% c("out", "in")) t = FALSE
  #     tectonicr::distance_from_pb(stress_df_filt0(), por(), plate2dist, tangential = t, km = TRUE)
  #   } else {
  #     NULL
  #   }
  # })
  #
  # stress_df_filt <-  reactive({
  #   if(is.null(pb_distance())){
  #     stress_df_filt0()
  #   } else {
  #   stress_df_filt0() |>
  #     mutate(pbdist = pb_distance()) |>
  #     filter(
  #       between(pbdist, input$pb_dist_filt[1], input$pb_dist_filt[2])
  #     )
  #   }
  # })

  output$interact_map <- renderGirafe({




    x <- stress_df_filt()

    if (is.null(por()) | !input$por_crs) {
      # ggplot(data = x, aes(lon, lat, angle = angle_map, color = regime, radius = radius_map)) +
      #   geom_sf(data = land, inherit.aes = FALSE) +
      #   geom_sf(data = plates2plot(), color = "red", inherit.aes = FALSE) +
      #   geom_sf(data = lcc, color = "#56B4E9", lwd = .5, alpha = .9, lty = 4, inherit.aes = FALSE) +
      #   geom_sf(data = lc, color = "#56B4E9", lwd = .5, alpha = .9, lty = 3, inherit.aes = FALSE) +
      #   geom_sf(data = gc, color = "#56B4E9", lwd = .5, alpha = .9, lty = 2, inherit.aes = FALSE) +
      #   geom_sf(data = sc, color = "#56B4E9", lwd = .5, alpha = .9, lty = 1, inherit.aes = FALSE) +
      #
      #   geom_spoke(data = stress_df, position = "center_spoke", alpha = .2, inherit.aes = TRUE) +
      #   geom_spoke(position = "center_spoke") +
      #   scale_color_manual("Stress regime", values = stress_colors()) +
      #   theme_bw() +
      #   labs(x = "", y = "") +
      #   coord_sf(xlim = range(x$lon), ylim = range(x$lat), expand = FALSE)

      mi <- ggplot(data = x, aes(lon, lat, angle = angle_map, radius = radius_map, tooltip = locality, data_id = id)) +
        geom_sf(data = land, inherit.aes = FALSE, lwd = .125) +
        geom_sf(data = plates2plot(), color = "red", lwd = .4, inherit.aes = FALSE) +
        geom_sf(data = trajectories(), aes(lty = type), color = "#56B4E9", lwd = .25, alpha = .9, inherit.aes = FALSE) +
        scale_linetype("Stress trajectory") +

        #geom_spoke(data = stress_df, position = "center_spoke", alpha = .2, inherit.aes = TRUE) +
        geom_spoke_interactive(aes(color = regime), size = .5, position = "center_spoke", hover_nearest = FALSE) +

        scale_color_manual("Stress regime", values =  stress_colors()) +
          theme_bw() +
          labs(x = "", y = "") +
        #theme(legend.position = "bottom") +
        coord_sf(xlim = range(x$lon), ylim = range(x$lat), expand = FALSE)


      girafe(ggobj = mi,
             options = list(
                 opts_selection(
                     type = "multiple",
                     selected = 1:nrow(x)
                     )#,
                 #opts_tooltip(use_fill = TRUE),
                 #opts_hover_inv(css = "opacity:0.1;"),
                 #opts_hover(css = "stroke-width:2;"),
                 #opts_sizing(rescale = TRUE),
                # opts_zoom(max = 5)
             )
      )



    } else {
      # PoR transformation and map
      x_por <- PoR_coordinates(x, por())
      x_por$azi_por <- PoR_shmax(x, por())
      x_por$azi_por_map <- tectonicr::deg2rad(90 - x_por$azi_por)
      x_por$radius_map <- x$radius_map
      x_por$regime <- x$regime

      # land_por <- geographical_to_PoR_sf(land, por) |>
      #   sf::st_crop(xmin = min(stress_df_2_filt_por$lon.PoR)-2, ymin = min(stress_df_2_filt_por$lat.PoR)-2, xmax = max(stress_df_2_filt_por$lon.PoR)+2, ymax = max(stress_df_2_filt_por$lat.PoR)+2)

      # plates2plot_por <- geographical_to_PoR_sf(plates2plot(), por)|>
      #   sf::st_crop(xmin = min(stress_df_2_filt_por$lon.PoR)-2, ymin = min(stress_df_2_filt_por$lat.PoR)-2, xmax = max(stress_df_2_filt_por$lon.PoR)+2, ymax = max(stress_df_2_filt_por$lat.PoR)+2)

      ggplot() +
        # geom_sf(data = land_por) +
        # geom_sf(data = plates2plot_por(), color = "red") +
        {
          if ("lc" %in% input$traj_filt) geom_abline(intercept = seq(-360, 360, 10), slope = rep(1, 73), color = "#56B4E9", lwd = .5, alpha = .9, lty = 3)
        } +
        {
          if ("lcc" %in% input$traj_filt) geom_abline(intercept = seq(-360, 360, 10), slope = rep(-1, 73), color = "#56B4E9", lwd = .5, alpha = .9, lty = 4)
        } +
        {
          if ("gc" %in% input$traj_filt) geom_vline(xintercept = seq(-180, 180, 10), lty = 2, lwd = .5, alpha = .9, color = "#56B4E9")
        } +
        {
          if ("sc" %in% input$traj_filt) geom_hline(yintercept = seq(-90, 90, 10), lty = 1, lwd = .5, alpha = .9, color = "#56B4E9")
        } +
        geom_spoke(data = x_por, aes(lon.PoR, lat.PoR, angle = azi_por_map, color = regime, radius = radius_map), position = "center_spoke") +
        scale_color_manual("Tectonic regime", values = stress_colors()) +
        theme_bw() +
        labs(x = "", y = "") +
        #coord_sf(xlim = range(x_por$lon.PoR), ylim = range(x_por$lat.PoR), expand = FALSE)
        coord_map(xlim = range(x_por$lon.PoR), ylim = range(x_por$lat.PoR), expand = FALSE)
    }


  })


  output$rose <- renderPlot({
    req(input$interact_map_selected)

    selected_rows <- readr::parse_number(input$interact_map_selected)

    x <- stress_df_filt() |>
      slice(selected_rows)

   # x = input$interact_map_selected


    if (is.null(por())) {
      rose(x$azi, x$unc, main = "Shmax orientation")
    } else {
      azi_PoR <- PoR_shmax(x, por(), input$prd_type)
      if (input$prd_type == "none") {
        rose(azi_PoR, x$unc, mtext = paste(input$plate_rot, "wrt.", input$plate_fix), main = "Shmax orientation")
      } else {
        rose(azi_PoR[, 1], x$unc, mtext = paste(input$plate_rot, "wrt.", input$plate_fix), main = "Shmax orientation")
        rose_line(azi_PoR$prd[1], col = "#56B4E9", radius = 1.1)
      }
    }
  })


  # output$distance_plot<- renderPlot({
  #   if(!is.null(por())){
  #   x <- stress_df_filt()
  #   x$azi_PoR <- PoR_shmax(x, por(), input$prd_type)
  #
  #   ggplot(x, aes(pbdist, azi_PoR, dolor = regime)) +
  #     coord_cartesian(ylim = c(0, 180)) +
  #     labs(x = "Distance from plate boundary (km)", y = "Azimuth in PoR (\u00B0)") +
  #     geom_hline(yintercept = c(0, 45, 90, 135, 180), lty = 3) +
  #     geom_pointrange(
  #       aes(
  #         ymin = azi_PoR - unc, ymax = azi_PoR + unc
  #       ),
  #       size = .25
  #     ) +
  #     scale_y_continuous(
  #       breaks = seq(-180, 360, 45),
  #       sec.axis = sec_axis(
  #         ~.,
  #         name = NULL,
  #         breaks = c(0, 45, 90, 135, 180),
  #         labels = c("Outward", "Tan (L)", "Inward", "Tan (R)", "Outward")
  #       )
  #     ) +
  #     scale_alpha_discrete(name = "Quality rank", range = c(1, 0.1)) +
  #     scale_color_manual(name = "Tectonic regime", values = stress_colors())
  #   }
  # })




  output$stats <- renderPrint({
   req(input$interact_map_selected)

    selected_rows <- readr::parse_number(input$interact_map_selected)

    x <- stress_df_filt() |>
      slice(selected_rows)


    #x <- stress_df_filt()
    w = 1 / x$unc
    n = nrow(x)

    if (is.null(por())) {
      mean <- circular_mean(x$azi, w)
      median <- circular_median(x$azi, w)
      sd <- circular_sd(x$azi,w)
      ci <- confidence_angle(x$azi, w = w)
      prd <- NA
      disp <- NA
    } else {
      azi_PoR <- PoR_shmax(x, por(), input$prd_type)
      if (input$prd_type == "none") {
        mean <- circular_mean(azi_PoR, w)
        median <- circular_median(azi_PoR, w)
        sd <- circular_sd(azi_PoR, w)
        ci <- confidence_angle(azi_PoR, w = w)
        prd <- NA
        disp <- NA
      } else {
        mean <- circular_mean(azi_PoR[, 1], w)
        median <- circular_median(azi_PoR[, 1], w)
        sd <- circular_sd(azi_PoR[, 1], w)
        ci <- confidence_angle(azi_PoR[, 1], w = w)
        prd <- azi_PoR$prd[1]
        disp <- circular_dispersion(azi_PoR[, 1], prd, w = w)
      }
    }

    data.frame(
      value = c(n, mean, median, sd, ci, prd, disp),
      row.names = c("n", "mean", "median", "sd", "95% confidence", "predicted", "dispersion")
    )
  })
}
