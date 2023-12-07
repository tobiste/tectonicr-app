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

  output$interact_map <- renderPlot({
    #regime_filt <- gsub("U", NA, input$regime_filt)
    #plates2plot <- plates[[input$plate_boundary_choice]]


    # model <- cpm_models |>
    #   filter(
    #     model == input$motion_choice
    #   )

    # if (input$plate_fix != "Enter plate..." & input$plate_rot != "Enter plate...") {
    #   por <- equivalent_rotation(model(), input$plate_fix, input$plate_rot)
    # } else {
    #   por <- NULL
    # }

    if (!is.null(por())) {
      if ("sc" %in% input$traj_filt) {
        sc <- eulerpole_smallcircles(por(), n = 36)
      } else {
        sc <- NULL
      }
      if ("gc" %in% input$traj_filt) {
        gc <- eulerpole_greatcircles(por(), n = 36)
      } else {
        gc <- NULL
      }
      if ("lc" %in% input$traj_filt) {
        lc <- eulerpole_loxodromes(por(), cw = TRUE, n = 36)
      } else {
        lc <- NULL
      }
      if ("lcc" %in% input$traj_filt) {
        lcc <- eulerpole_loxodromes(por(), cw = FALSE, n = 36)
      } else {
        lcc <- NULL
      }
      # trajectories <- rbind(sc, gc, lc, lcc)
    } else {
      sc <- gc <- lc <- lcc <- land[0]
    }

    # stress_df_2_filt <- stress_df |>
    #   filter(
    #     between(unc, input$quality_filt[1], input$quality_filt[2]),
    #     between(depth, input$depth_filt[1], input$depth_filt[2]),
    #     between(lat, input$lat_filt[1], input$lat_filt[2]),
    #     between(lon, input$lon_filt[1], input$lon_filt[2]),
    #     regime %in% regime_filt()
    #   )

    x <- stress_df_filt()

    if (is.null(por()) | !input$por_crs) {
      ggplot() +
        geom_sf(data = land) +
        geom_sf(data = plates2plot(), color = "red") +
        geom_sf(data = lcc, color = "#56B4E9", lwd = .5, alpha = .9, lty = 4) +
        geom_sf(data = lc, color = "#56B4E9", lwd = .5, alpha = .9, lty = 3) +
        geom_sf(data = gc, color = "#56B4E9", lwd = .5, alpha = .9, lty = 2) +
        geom_sf(data = sc, color = "#56B4E9", lwd = .5, alpha = .9, lty = 1) +
        geom_spoke(data = x, aes(lon, lat, angle = angle_map, color = regime, radius = radius_map), position = "center_spoke") +
        scale_color_manual("Stress regime", values = stress_colors()) +
        theme_bw() +
        labs(x = "", y = "") +
        coord_sf(xlim = range(x$lon), ylim = range(x$lat), expand = FALSE)
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
        scale_color_manual("Stress regime", values = stress_colors()) +
        theme_bw() +
        labs(x = "", y = "") +
        coord_sf(xlim = range(x_por$lon.PoR), ylim = range(x_por$lat.PoR), expand = FALSE)
    }

    # mi <- ggplot() +
    #     geom_sf(data = land) +
    #     geom_sf(data = plates, color = "red") +
    #     geom_spoke_interactive(data = stress_df_2_plot, aes(lon, lat, angle =angle_map, color = regime, radius = radius_map, tooltip = locality, data_id = id), hover_nearest = FALSE, position = "center_spoke") +
    #     scale_color_manual("Stress regime", values =  stress_colors()) +
    #     theme_bw() +
    #     labs(x = "", y = "") +
    #     coord_sf(xlim = range(stress_df_2_plot$lon), ylim = range(stress_df_2_plot$lat), expand = FALSE)
    #
    #
    # girafe(ggobj = mi,
    #        options = list(
    #            opts_selection(
    #                type = "single",
    #                only_shiny = FALSE),
    #            opts_tooltip(use_fill = TRUE),
    #            opts_hover_inv(css = "opacity:0.1;"),
    #            opts_hover(css = "stroke-width:2;"),
    #            opts_zoom(max = 5)
    #        )
    # )
  })


  output$rose <- renderPlot({
    # regime_filt <- gsub("U", NA, input$regime_filt)
    # plates2plot <- plates[[input$plate_boundary_choice]]
    #
    #
    # model <- cpm_models |>
    #   filter(
    #     model == input$motion_choice
    #   )
    #
    # if (input$plate_fix != "Enter plate..." & input$plate_rot != "Enter plate...") {
    #   por <- equivalent_rotation(model, input$plate_fix, input$plate_rot)
    # } else {
    #   por <- NULL
    # }
    #
    # stress_df_filt <- stress_df |>
    #   filter(
    #     between(unc, input$quality_filt[1], input$quality_filt[2]),
    #     between(depth, input$depth_filt[1], input$depth_filt[2]),
    #     between(lat, input$lat_filt[1], input$lat_filt[2]),
    #     between(lon, input$lon_filt[1], input$lon_filt[2]),
    #     regime %in% regime_filt
    #   )

    x <- stress_df_filt()

    if (is.null(por())) {
      rose(x$azi, x$unc, main = "Shmax Orientation")
    } else {
      azi_PoR <- PoR_shmax(x, por(), input$prd_type)
      if (input$prd_type == "none") {
        rose(azi_PoR, x$unc, mtext = paste(input$plate_rot, "wrt.", input$plate_fix), main = "Shmax Orientation")
      } else {
        rose(azi_PoR[, 1], x$unc, mtext = paste(input$plate_rot, "wrt.", input$plate_fix), main = "Shmax Orientation")
        rose_line(azi_PoR$prd[1], col = "#56B4E9")
      }
    }
  })

  output$stats <- renderPrint({
    # regime_filt <- gsub("U", NA, input$regime_filt)
    # plates2plot <- plates[[input$plate_boundary_choice]]


    # model <- cpm_models |>
    #   filter(
    #     model == input$motion_choice
    #   )
    #
    # if (input$plate_fix != "Enter plate..." & input$plate_rot != "Enter plate...") {
    #   por <- equivalent_rotation(model, input$plate_fix, input$plate_rot)
    # } else {
    #   por <- NULL
    # }

    # stress_df_filt <- stress_df |>
    #   filter(
    #     between(unc, input$quality_filt[1], input$quality_filt[2]),
    #     between(depth, input$depth_filt[1], input$depth_filt[2]),
    #     between(lat, input$lat_filt[1], input$lat_filt[2]),
    #     between(lon, input$lon_filt[1], input$lon_filt[2]),
    #     regime %in% regime_filt
    #   )

    x <- stress_df_filt()

    if (is.null(por())) {
      mean <- circular_mean(x$azi, x$unc)
      median <- circular_median(x$azi, x$unc)
      sd <- circular_sd(x$azi, x$unc)
      prd <- NA
      disp <- NA
    } else {
      azi_PoR <- PoR_shmax(x, por(), input$prd_type)
      if (input$prd_type == "none") {
        mean <- circular_mean(azi_PoR, x$unc)
        median <- circular_median(azi_PoR, x$unc)
        sd <- circular_sd(azi_PoR, x$unc)
        prd <- NA
        disp <- NA
      } else {
        mean <- circular_mean(azi_PoR[, 1], x$unc)
        median <- circular_median(azi_PoR[, 1], x$unc)
        sd <- circular_sd(azi_PoR[, 1], x$unc)
        prd <- azi_PoR$prd[1]
        disp <- circular_dispersion(azi_PoR[, 1], prd, x$unc)
      }
    }

    structure(
      c(mean, median, sd, prd, disp),
      names = c("mean", "median", "sd", "predicted", "dispersion")
    )
  })
}
