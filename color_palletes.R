#Color palettes

library(viridis)
library(RColorBrewer)
#install.packages("Polychrome")
library(Polychrome)

# build-in color palette
Glasbey = glasbey.colors(32)
swatch(Glasbey)

# COMBINING COLOR PALS
nb.cols <- 50  # Define the number of colors you want
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

nb.cols <- 32
pal = c(brewer.pal(5, "RdYlBu"), brewer.pal(5, "PRGn"), brewer.pal(5, "BrBG"))
mycolors <- colorRampPalette(pal)(nb.cols)
swatch(mycolors)


nb.cols <- 32
pal = c(brewer.pal(10, "Paired"), brewer.pal(5, "RdBu"), 
        brewer.pal(5, "PuOr"), brewer.pal(5, "PiYG"), 
        brewer.pal(5, "BrBG"))
mycolors <- colorRampPalette(pal)(nb.cols)
swatch(mycolors)

colors81 <- c('#3CB371', '#F4F4F4', '#A0522D', '#8B5A00', '#7CFC00', '#8B1C62', '#A4D3EE', '#EEA9B8', '#EEDFCC', '#3D9140', 
              '#CD6839', '#800080', '#696969', '#EECBAD', '#9932CC', '#CD1076', '#EE82EE', '#6959CD', '#00C5CD', '#FFA07A', 
              '#EEE9BF', '#CDB79E', '#CDC5BF', '#FFF68F', '#EECFA1', '#79CDCD', '#7AC5CD', '#8B475D', '#B3EE3A', '#CD2626', 
              '#FF4500', '#FF3E96', '#7D9EC0', '#FF7F50', '#4F94CD', '#EED5B7', '#ADD8E6', '#8B7E66', '#CD5B45', '#BBFFFF', 
              '#CD0000', '#C0C0C0', '#EEE0E5', '#FFFACD', '#808069', '#FFC125', '#EE8262', '#6A5ACD', '#D15FEE', '#555555', 
              '#848484', '#FFE7BA', '#8B3A62', '#8B6508', '#E9967A', '#8E8E38', '#8B8386', '#EEEE00', '#76EEC6', '#EEEED1', 
              '#8E388E', '#4EEE94', '#00CED1', '#BCD2EE', '#9AC0CD', '#0000CD', '#90EE90', '#C71585', '#B8860B', '#87CEEB', 
              '#CD4F39', '#00EE00', '#CDB38B', '#A52A2A', '#F0F8FF', '#7A67EE', '#8B4C39', '#FF1493', '#483D8B', '#F0FFFF', 
              '#191970')


colors152 <- c("#5D8AA8", "#F4C430",  "#A4C639", "#CC0000", '#A4D3EE', 
               "#4B5320", "#F5F5DC", "#DE5D83", "#FFC1CC", 
                "#E9D66B" ,"#B2BEB5", "#87A96B" ,"#FF9966", 
               "#6D351A" ,  "#BF94E4", "#3B444B", 
                "#848482", "#98777B" , "#3D2B1F", 
                "#318CE7", "#FAF0BE", '#3CB371',
                "#79443B", "#B5A642",  
               "#C32148", "#008080", "#FF007F", "#08E8DE", 
                "#004225", "#CD7F32", "#964B00","#F0DC82", 
               "#800020", "#DEB887", "#CC5500", "#E97451", "#8A3324",
                "#BD33A4", "#702963", "#536878", "#7FFFD4", "#006B3C", "#ED872D", 
               "#E30022", "#A3C1AD", "#78866B", "#FFEF00", "#FF0800",
                "#C41E3A", "#00CC99", "#99BADD", "#ED9121", "#92A1CF", 
               "#ACE1AF", "#007BA7", "#2A52BE", "#A0785A", "#36454F",
                "#DFFF00", "#DE3163", "#FFB7C5", "#7B3F00", "#98817B", 
               "#D2691E", "#E4D00A", "#FBCCE7", "#00FF6F", 
                "#0047AB", "#9BDDFF", "#002E63", "#8C92AC", "#B87333", 
               "#FF3800", "#FF7F50", "#B31B1B", "#6495ED", "#DC143C",
                "#00008B", "#654321", "#5D3954", "#A40000", "#08457E", 
               "#CD5B45", "#B8860B", "#013220", "#BDB76B", "#734F96",
                "#8B008B", "#556B2F", "#FF8C00", "#779ECB", "#03C03C", 
               "#966FD6", "#003399", "#872657", "#E9967A", "#9400D3",
                "#1560BD", "#C19A6B", "#EDC9AF", "#696969", "#85BB65", 
               "#00009C", "#E1A95F", "#FF1C00", "#FC8EAC", "#F7E98E",
                "#6082B6", "#FF5C5C", "#00A86B", "#C3B091", "#B57EDC", 
               "#CCCCFF", "#7CFC00", "#FF00FF", "#000080", "#808000",
                "#002147", "#FF6961", "#CB99C9", "#D1E231", "#FF5A36", 
               "#69359C", "#FF6700", "#967117", "#A7FC00", "#00755E",
                "#30D5C8", "#120A8F", "#5B92E5", "#F3E5AB", 
               "#738678", "#0F4D92", "#4682B4", "#FAD6A5", "#FFFF00",
                "#008080", "#E4D96F")



colors52 = c('#B0171F', '#EEA2AD', '#8B5F65', '#CD3278', 
           '#DA70D6', '#FFE1FF', '#7D26CD', '#473C8B', 
           '#0000EE', '#436EEE', '#6E7B8B', '#CAE1FF', 
           '#00BFFF', '#00E5EE', '#00868B', '#00C78C', 
           '#7FFFD4', '#00CD66', '#2E8B57', '#C1CDC1', 
           '#6E8B3D', '#9ACD32', '#7CFC00', '#FFF68F', 
           '#CDC673', '#8B864E', '#FFD700', '#FFA500', 
           '#8B5A00', '#FFA07A', '#EE4000', '#8B3626', 
           '#800000', '#D8BFD8', '#CD00CD', '#800080', 
           '#551A8B', '#FF7F24', '#63B8FF', '#1874CD', 
           '#191970', '#836FFF', '#292421', '#CD5B45', 
           '#424242', '#FF00FF', '#00CD00', '#CD5555', 
           '#FFC1C1', '#C67171', '#006400', '#228B22')


# discrete color
# install.packages("pals")
library(pals)
pal.bands(alphabet, alphabet2, cols25, glasbey, kelly, polychrome, 
          stepped, tol, watlington,
          show.names=FALSE)





