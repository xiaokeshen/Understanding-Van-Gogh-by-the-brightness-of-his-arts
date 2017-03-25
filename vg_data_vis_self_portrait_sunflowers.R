

zz = read.delim("van_gogh_all.txt")
#filter the genre_gen which does not contain other
#yy=sunflowers
yy<-zz %>% filter((str_detect(Title,"Sunflowers")
&str_detect(Genre_gen, "still-life"))
|str_detect(Genre_gen, "self-portrait"))
yy$Genre_gen <- gsub('still-life', 'Sunflowers(still life)', yy$Genre_gen)
yy$Genre_gen <- gsub('self-portrait', 'self portrait', yy$Genre_gen)
#yy[yy=="still-life"]<-"Sunflowers"
#program for the first graph
kk<-ggplot(yy, aes(x= Year_Month, y= Brightness_Median,color= Brightness_Median)) + geom_point(size=yy$Image_Width* yy$Image_Height/70000,alpha=1)+ facet_wrap(~ Genre_gen)+  scale_colour_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),values=c(1.0,0.8,0.6,0.4,0.2,0),guide_legend(title ="Brightness"))+labs(title="Brightness for self portrait and Sunflowers ", y="Brightness", x="Time")+ theme(legend.position = c(0.92, 0.14))+ theme_minimal()
ggsave("brightnessForSunflowers.png", kk)

#select the self portrait data
self_portrait_data<-yy %>% filter(str_detect(Genre_gen, "self portrait"))
self_portrait<-ggplot(self_portrait_data, aes(x= Year_Month, y= Brightness_Median,color= Brightness_Median)) + geom_point(size=self_portrait_data$Image_Width* self_portrait_data$Image_Height/70000,alpha=1)+  scale_colour_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),values=c(1.0,0.8,0.6,0.4,0.2,0),guide_legend(title ="Brightness"))+labs(title="Brightness for self portrait", y="Brightness", x="Time")+ theme(legend.position = c(0.92, 0.14))+ theme_minimal()
ggsave("self_portrait.png", self_portrait)

#select the Sunflowers data
sunflowers_data<-yy %>% filter(str_detect(Title,"Sunflowers"))
sunflowers<-ggplot(sunflowers_data, aes(x= Year_Month, y= Brightness_Median,color= Brightness_Median)) + geom_point(size=sunflowers_data$Image_Width* sunflowers_data$Image_Height/70000,alpha=1)+  scale_colour_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),values=c(1.0,0.8,0.6,0.4,0.2,0),guide_legend(title ="Brightness"))+labs(title="Brightness for Sunflowers", y="Brightness", x="Time")+ theme(legend.position = c(0.92, 0.14))+ theme_minimal()
ggsave("sunflowers.png", sunflowers)


#program for the second graph
test=aggregate(yy, by = list(yy$Year_Month,yy$Genre_gen), FUN =mean)

mm<-ggplot(test, aes(x= Group.1, y= Brightness_Median,color= Brightness_Median)) + geom_point(size=2,alpha=1)+geom_smooth(size=0.6,linetype = "dotted",method="loess", se=F) + xlim(c(1886.250, 1889.000)) + ylim(c(50,200))+ facet_wrap(~ Group.2)+    scale_colour_gradientn(colours = c("red","yellow","green","lightblue","darkblue"),                      values=c(1.0,0.8,0.6,0.4,0.2,0),guide_legend(title ="Brightness")) +labs(title="Average Brightness for for self portrait and Sunflowers with trend", y="Brightness", x="Time")+ theme(legend.position = c(0.92, 0.14))+ theme_minimal()
ggsave("brightnessSunflowers_with_trend.png", mm)
