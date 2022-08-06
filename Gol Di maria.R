## Selecciona todo el código y presiona "Ctrl" + "Enter" para ejecutarlo
# si se demora un poco es porque se puso a instalar algún paquete (librería) que no tienes en tu PC
# si todo sale bien, debería aparecer el archivo PNG "gol_di_maria_copa_america" en la misma carpeta donde tienes el código


# Cargar paquetes, datos, funciones y definir variables necesarias
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, ggplot2, ggforce,ggtext)

dims <- list(
  length = 110,
  width = 73,
  penalty_box_length = 16.5,
  penalty_box_width = 40.32,
  six_yard_box_length = 5.5,
  six_yard_box_width = 18.21,
  penalty_spot_distance = 11,
  central_circle_radius = 9.15, 
  goal_width = 7.32,
  origin_x = 0,
  origin_y = 0
)


# función para dibujar cancha completa
get_pitch <- function(gp, dims, pitch_fill = "white", pitch_col = "grey70", background_fill = pitch_fill, margin = 0){
  
  contorno_df <- data.frame(x = dims$origin_x, 
                            xend = dims$length,
                            y = dims$origin_y,
                            yend = dims$width)
  
  x_start_areas <- c(dims$origin_x, dims$origin_x, dims$length - dims$penalty_box_length, dims$length - dims$six_yard_box_length)
  
  x_end_areas <- c(dims$penalty_box_length, dims$six_yard_box_length, dims$length, dims$length)
  
  y_start_areas <- c((dims$width - dims$penalty_box_width)/2, (dims$width - dims$six_yard_box_width)/2, (dims$width - dims$penalty_box_width)/2, (dims$width - dims$six_yard_box_width)/2)
  
  y_end_areas <- c(dims$width - (dims$width - dims$penalty_box_width)/2, dims$width - (dims$width - dims$six_yard_box_width)/2, dims$width - (dims$width - dims$penalty_box_width)/2, dims$width - (dims$width - dims$six_yard_box_width)/2)
  
  areas_df<- data.frame(x = x_start_areas, 
                        xend = x_end_areas, 
                        y = y_start_areas,
                        yend = y_end_areas)
  
  gp +
    theme_void() +
    theme(panel.background = element_rect(fill = background_fill, colour = "transparent"),
          plot.margin = unit(c(margin, margin, margin, margin), "cm")) +
    
    # rectángulos
    #áreas
    geom_rect(data = contorno_df,
              aes(xmin = x, xmax = xend, ymin = y, ymax = yend), col = pitch_col, fill = pitch_fill) +
    geom_rect(data = areas_df,
              aes(xmin = x, xmax = xend, ymin = y, ymax = yend), col = pitch_col, fill = pitch_fill) +
    #porterías
    geom_rect(aes(xmin = dims$length, xmax = dims$length + 1.5, ymin = dims$width/2 - dims$goal_width/2, ymax = dims$width/2 + dims$goal_width/2), 
              fill = pitch_col, col = pitch_col) +
    geom_rect(aes(xmin = dims$origin_x, xmax = dims$origin_x - 1.5, ymin = dims$width/2 - dims$goal_width/2, ymax = dims$width/2 + dims$goal_width/2), 
              fill = pitch_col, col = pitch_col) +
    
    # puntos
    geom_point(aes(x = dims$length/2, y = dims$width/2), col = pitch_col) +
    geom_point(aes(x = dims$length - dims$penalty_spot_distance, y = dims$width/2), col = pitch_col) +
    geom_point(aes(x = dims$penalty_spot_distance, y = dims$width/2), col = pitch_col) +
    
    #círculo central
    geom_circle(aes(x0 = dims$length/2, y0 = dims$width/2, r = dims$central_circle_radius), color = pitch_col) +
    #línea central
    geom_segment(aes(x = dims$length/2, xend = dims$length/2, y = dims$width, yend = dims$origin_y), color = pitch_col) +
    
    # semi círculos áreas
    geom_arc(aes(x0 = dims$length - dims$penalty_spot_distance, y0 = dims$width/2, r = dims$central_circle_radius, 
                 start = -37*pi/180, end = -143*pi/180), col = pitch_col) +
    geom_arc(aes(x0 = dims$penalty_spot_distance, y0 = dims$width/2, r = dims$central_circle_radius, 
                 start = 37*pi/180, end = 143*pi/180), col = pitch_col) +
    
    # semi círculos corners
    geom_arc(aes(x0 = dims$length, y0 = dims$origin_y, r = 1, 
                 start = 270*pi/180, end = 360*pi/180), col = pitch_col) +
    geom_arc(aes(x0 = dims$length, y0 = dims$width, r = 1, 
                 start = 180*pi/180, end = 270*pi/180), col = pitch_col) +
    geom_arc(aes(x0 = dims$origin_x, y0 = dims$origin_y, r = 1, 
                 start = 0*pi/180, end = 90*pi/180), col = pitch_col) +
    geom_arc(aes(x0 = dims$origin_x, y0 = dims$width, r = 1, 
                 start = 90*pi/180, end = 180*pi/180), col = pitch_col)
}
#----------------------

#carga datos
data <- read_csv("C:/Users/fede/Desktop/Facultad/Optativas/Seminario II ( Inv. Modelización matemática y datos)/YDRAY-data_gol_di_maria-La-Pizarra-del-DT.csv")

View(data)

# Visualización
text_lines_color = "grey70"
fill_color = "#525252"
events_fill_color = "#00AAE4"

get_pitch(gp = ggplot(), dims = dims, margin = 0.6, pitch_col = "grey70", pitch_fill = fill_color) +
  annotate("segment", x = 13, xend = 0, y = 48, yend = 35, col = "#FFD700", linetype = 1, size = 0.8, 
           arrow = arrow(type = "closed", length = unit(0.07, "inches"))) +
  annotate("curve", x = 80, xend = 20, y = 66, yend = 53, col = "white", linetype = 2, curvature = 0.15) +
  annotate("text", x = 38, y = 67, label = "Movimiento sin balón\nde Di María", col = "white", angle = 0) +
  geom_segment(data = data %>% filter(event.type_name == "pass"),
               aes(x = pos_x_meter, y = pos_y_meter, xend = pass_end_pos_x_meter, yend = pass_end_pos_y_meter),
               alpha = 0.5, size = 0.8, col = events_fill_color) +
  geom_segment(data = data %>% filter(event.type_name == "carrera"),
               aes(x = pos_x_meter, y = pos_y_meter, xend = carrera_end_pos_x_meter, yend = carrera_end_pos_y_meter),
               alpha = 0.5, size = 1, col = events_fill_color, linetype = 3) +
  geom_point(data = data %>% filter(event.type_name %in% c("shot", "pass", "ball receipt") & lag(event.type_name) != "recover"),
             aes(x = pos_x_meter, y = pos_y_meter), size = 8,  
             pch = 21, fill = events_fill_color, stroke = 1.3, col = "white") +
  geom_point(data = data %>% filter(event.type_name %in% c("recover")),
             aes(x = pos_x_meter, y = pos_y_meter), size = 7,  
             pch = 23, fill = "#fd8d3c", stroke = 1.3, col = "white") +
  geom_text(data = data %>% filter(event.type_name %in% c("shot", "pass", "ball receipt", "recover") & (lag(event.type_name) != "recover" | is.na(lag(event.type_name)))),
            aes(x = pos_x_meter, y = pos_y_meter, label = player.number), size = 4, col = "white") +
  annotate("segment", x = 65, xend = 45, y = -5, yend = -5, col = text_lines_color, 
           arrow = arrow(type = "closed", length = unit(0.1, "inches"))) +
  annotate("text", x = 56, y = -8, label = "Dirección de juego", col = text_lines_color) +
  annotate("text", x = 45, y = 45.5, label = "Pase", col = events_fill_color, angle = -18) +
  annotate("text", x = 75, y = 45, label = "Conducción", col = events_fill_color, angle = 0) +
  annotate("text", x = 81, y = 70.5, label = "Recuperación", col = "#fd8d3c", angle = 0) +
  labs(title = "<b style='color: #FFD700'>Gol <b style='color: grey70'>de <b style='color: white'>Ángel Di María <b style='color: grey70'>para <b style='color: #00AAE4'>Argentina <b style='color: grey70'>en la final de Copa América 2021",
       subtitle = "Minuto 22 / Distancia: 18.5 metros / Valor xG = 0.1",
       caption = "Data: recopilación manual LPDT \nDiseño inspirado en: @Odriozolite") +
  theme(text = element_text(colour = text_lines_color, size = 12),
        plot.title = element_markdown(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, vjust = 1, face= "italic"),
        plot.background = element_rect(fill = fill_color, colour = "transparent"),
        plot.margin = margin(0.5, 0.3, 0.25, 0.3, "cm"),
        legend.position = "none") +
  annotate("point", x = 3, y = c(-3, -7, -11), pch = 21, fill = events_fill_color, col = "white", size = 6) +
  annotate("text", x = 2.2, y = -3, col = "white", size = 2.5, label = "11   A. Di María", hjust = 0) +
  annotate("text", x = 2.5, y = -7, col = "white", size = 2.5, label = "7    R. de Paul", hjust = 0) +
  annotate("text", x = 2.2, y = -11, col = "white", size = 2.5, label = "19   N. Otamendi", hjust = 0)






