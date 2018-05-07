library(ggplot2)
data(father.son, package='UsingR')

ggplot(father.son, aes(x=fheight, y=sheight)) + 
    geom_point() + 
    geom_smooth(method='lm')

heightMod <- lm(sheight ~ fheight, data=father.son)
