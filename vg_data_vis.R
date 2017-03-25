van_gogh_original_data = read.delim("van_gogh_all.txt")
#filter the genre_gen which does not contain other
van_gogh_drop_other<-van_gogh_original_data %>%
filter(!str_detect(Genre_gen, "other")&!str_detect(Genre_gen, "genre scene"))

#replace the "_" and "-" with " "
van_gogh_drop_other$Genre_gen <- gsub('_', ' ', van_gogh_drop_other$Genre_gen)
van_gogh_drop_other$Genre_gen <- gsub('-', ' ', van_gogh_drop_other$Genre_gen)

brightness_graph_whole<-ggplot(van_gogh_drop_other, aes(x= Year_Month, y= Brightness_Median,color= Brightness_Median)) + geom_point(size=van_gogh_drop_other$Image_Width* van_gogh_drop_other$Image_Height/70000,alpha=0.3)+  scale_colour_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),values=c(1.0,0.8,0.6,0.4,0.2,0),guide_legend(title ="Brightness"))+labs(title="Brightness of the whole arts", y="Brightness", x="Time")+ theme(legend.position =  "bottom")+ theme_minimal()
ggsave("brightnessForGenreSizeOfImageIsShownNew_whole.png", brightness_graph_whole)

#program for the first graph
brightness_graph_by_gene<-ggplot(van_gogh_drop_other, aes(x= Year_Month, y= Brightness_Median,color= Brightness_Median)) + geom_point(size=van_gogh_drop_other$Image_Width* van_gogh_drop_other$Image_Height/70000,alpha=0.3)+ facet_wrap(~ Genre_gen)+  scale_colour_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),values=c(1.0,0.8,0.6,0.4,0.2,0),guide_legend(title ="Brightness"))+labs(title="Brightness by Genre", y="Brightness", x="Time")+ theme(legend.position =  "bottom")+ theme_minimal()
ggsave("brightnessForGenreSizeOfImageIsShownNew_by_gene.png", brightness_graph_by_gene)



#program for the second graph
test=aggregate(van_gogh_drop_other, by = list(van_gogh_drop_other$Year_Month,van_gogh_drop_other$Genre_gen), FUN =mean)

brightness_graph_with_trend<-ggplot(test, aes(x= Group.1, y= Brightness_Median,color= Brightness_Median)) + geom_point(size=2,alpha=1)+geom_smooth(size=0.6,linetype = "dotted",method="loess", se=F) + xlim(c(1882.584, 1890.417)) + ylim(c(min(test$Brightness_Median, na.rm = TRUE),max(test$Brightness_Median, na.rm = TRUE)))+ facet_wrap(~ Group.2)+    scale_colour_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),                      values=c(1.0,0.8,0.6,0.4,0.2,0),guide_legend(title ="Brightness")) +labs(title="Average Brightness per month by Genre with trend", y="Brightness", x="Time")+ theme(legend.position =  "bottom")+ theme_minimal()
ggsave("brightnessForGenreNew.png", brightness_graph_with_trend)
