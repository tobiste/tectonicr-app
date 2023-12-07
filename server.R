function(input, output) {
  output$interact_map <- renderPlot({
    regime_filt <- gsub("U", NA, input$regime_filt)
    plates2plot <- plates[[input$plate_boundary_choice]]


    model <- cpm_models |>
      filter(
        model == input$motion_choice
      )

    if (input$plate_fix != "Enter plate..." & input$plate_rot != "Enter plate...") {
      por <- equivalent_rotation(model, input$plate_fix, input$plate_rot)
    } else {
      por <- NULL
    }

    if (!is.null(por)) {
      if ("sc" %in% input$traj_filt) {
        sc <- eulerpole_smallcircles(por)
      } else {
        sc <- NULL
      }
      if ("gc" %in% input$traj_filt) {
        gc <- eulerpole_greatcircles(por)
      } else {
        gc <- NULL
      }
      if ("lc" %in% input$traj_filt) {
        lc <- eulerpole_loxodromes(por, cw = TRUE)
      } else {
        lc <- NULL
      }
      if ("lcc" %in% input$traj_filt) {
        lcc <- eulerpole_loxodromes(por, cw = FALSE)
      } else {
        lcc <- NULL
      }
      # trajectories <- rbind(sc, gc, lc, lcc)
    } else {
      sc <- gc <- lc <- lcc <- land[0]
    }

    stress_df_2_filt <- stress_df |>
      filter(
        between(unc, input$quality_filt[1], input$quality_filt[2]),
        between(depth, input$depth_filt[1], input$depth_filt[2]),
        between(lat, input$lat_filt[1], input$lat_filt[2]),
        between(lon, input$lon_filt[1], input$lon_filt[2]),
        regime %in% regime_filt
      )


    ggplot() +
      geom_sf(data = land) +
      geom_sf(data = plates2plot, color = "red") +
      geom_sf(data = lcc, color = "blue", lwd = .1, alpha = .5, lty = 4) +
      geom_sf(data = lc, color = "blue", lwd = .1, alpha = .5, lty = 3) +
      geom_sf(data = gc, color = "blue", lwd = .1, alpha = .5, lty = 2) +
      geom_sf(data = sc, color = "blue", lwd = .1, alpha = .5, lty = 1) +
      geom_spoke(data = stress_df_2_filt, aes(lon, lat, angle = angle_map, color = regime, radius = radius_map), position = "center_spoke") +
      scale_color_manual("Stress regime", values = stress_colors()) +
      theme_bw() +
      labs(x = "", y = "") +
      coord_sf(xlim = range(stress_df_2_filt$lon), ylim = range(stress_df_2_filt$lat), expand = FALSE)

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
    regime_filt <- gsub("U", NA, input$regime_filt)
    plates2plot <- plates[[input$plate_boundary_choice]]


    model <- cpm_models |>
      filter(
        model == input$motion_choice
      )

    if (input$plate_fix != "Enter plate..." & input$plate_rot != "Enter plate...") {
      por <- equivalent_rotation(model, input$plate_fix, input$plate_rot)
    } else {
      por <- NULL
    }

    stress_df_filt <- stress_df |>
      filter(
        between(unc, input$quality_filt[1], input$quality_filt[2]),
        between(depth, input$depth_filt[1], input$depth_filt[2]),
        between(lat, input$lat_filt[1], input$lat_filt[2]),
        between(lon, input$lon_filt[1], input$lon_filt[2]),
        regime %in% regime_filt
      )

    if (is.null(por)) {
      rose(stress_df_filt$azi, stress_df_filt$unc, main = "Shmax Orientation")
    } else {
      azi_PoR <- PoR_shmax(stress_df_filt, por, input$prd_type)
      if (input$prd_type == "none") {
        rose(azi_PoR, stress_df_filt$unc, mtext = paste(input$plate_rot, "wrt.", input$plate_fix), main = "Shmax Orientation")
      } else {
        rose(azi_PoR[, 1], stress_df_filt$unc, mtext = paste(input$plate_rot, "wrt.", input$plate_fix), main = "Shmax Orientation")
        rose_line(azi_PoR$prd[1], col = "green")
      }
    }
  })

  output$stats <- renderPrint({
    regime_filt <- gsub("U", NA, input$regime_filt)
    plates2plot <- plates[[input$plate_boundary_choice]]


    model <- cpm_models |>
      filter(
        model == input$motion_choice
      )

    if (input$plate_fix != "Enter plate..." & input$plate_rot != "Enter plate...") {
      por <- equivalent_rotation(model, input$plate_fix, input$plate_rot)
    } else {
      por <- NULL
    }

    stress_df_filt <- stress_df |>
      filter(
        between(unc, input$quality_filt[1], input$quality_filt[2]),
        between(depth, input$depth_filt[1], input$depth_filt[2]),
        between(lat, input$lat_filt[1], input$lat_filt[2]),
        between(lon, input$lon_filt[1], input$lon_filt[2]),
        regime %in% regime_filt
      )

    if (is.null(por)) {
      mean <- circular_mean(stress_df_filt$azi, stress_df_filt$unc)
      median <- circular_median(stress_df_filt$azi, stress_df_filt$unc)
      sd <- circular_sd(stress_df_filt$azi, stress_df_filt$unc)
      prd <- NA
      disp <- NA
    } else {
      azi_PoR <- PoR_shmax(stress_df_filt, por, input$prd_type)
      if (input$prd_type == "none") {
        mean <- circular_mean(azi_PoR, stress_df_filt$unc)
        median <- circular_median(azi_PoR, stress_df_filt$unc)
        sd <- circular_sd(azi_PoR, stress_df_filt$unc)
        prd <- NA
        disp <- NA
      } else {
        mean <- circular_mean(azi_PoR[, 1], stress_df_filt$unc)
        median <- circular_median(azi_PoR[, 1], stress_df_filt$unc)
        sd <- circular_sd(azi_PoR[, 1], stress_df_filt$unc)
        prd <- azi_PoR$prd[1]
        disp <- circular_dispersion(azi_PoR[, 1], prd, stress_df_filt$unc)
      }
    }

    structure(
      c(mean, median, sd, prd, disp),
      names = c("mean", "median", "sd", "predicted", "dispersion")
    )
  })
}
