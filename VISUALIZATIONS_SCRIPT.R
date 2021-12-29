setwd("~/Desktop/FACULTY_DATA")
devtools::install_github("hrbrmstr/streamgraph")
library(streamgraph)
library(htmlwidgets)
library(plotly)

### Plot associated graphs for visualitzation 1, save as HTML widgets #############

#Cornell Engineering Student to Faculty Ratio, 1875-1915
plot1a <- ggplot(Professors, aes(x=as.numeric(Year), y=student_faculty_ratio))+geom_point(aes(color="Red"))+geom_line()+ggtitle("Cornell Engineering Student to Faculty Ratio, 1875-1915")+xlab("Year")+ylab("Student to Faculty Ratio")
plot1a <- ggplotly(plot_1a)
saveWidget(plot_1a, file="plot1a.html")
plot1a

#Number of Cornell Students and Faculty in the Engineering Programs by Year 1875-1915

plot1b<-ggplot(tablec, aes(x=Year, y=Number, color=Type))+geom_point()+geom_line(aes(group=Type))+ggtitle("Number of Cornell Students and Faculty in the Engineering Programs by Year 1875-1915")+xlab("Year")+ylab("Number")
plot1b<- ggplotly(plot1b)
saveWidget(plot1b, file="plot1b.html")
plot1b

#Cornell Total and Engnineering Students By Year, 1875-1915
plot1c<-ggplot(tabled, aes(x=Year, y=Number, color=Type))+geom_point()+geom_line(aes(group=Type))+ggtitle("Cornell Total and Engnineering Students By Year, 1875-1915")+xlab("Year")+ylab("Students Enrolled")
plot1c <- ggplotly(plot1c)
saveWidget(plot1c, file="plot1c.html")
plot1c

#Plot associated graphs for visualization 2, save as widgets

plot2 <- ggplot(RETURN_PROFS, aes(x=YEAR, y=freq, fill=Status))+geom_col(position="stack")+ggtitle("Returning vs. New Engineering Faculty at Cornell, 1880-1915")+xlab("Year")+ylab("Number")
plot2

#plot associated graphs for visualization 3, save as HTML widgets
plot3a <- ggplot(Department, aes(x=Year, y=freq, fill=Department))+geom_col(position="stack")+ggtitle("Cornell Engineering Faculty By Department, 1875-1915")+ylab("Number")
plot3a <- ggplotly(plot3a)
plot3a
saveWidget(plot3a, file="plot3a.html")

plot3b <- streamgraph(Department, key="Department", value="freq", date="Year", interpolate="linear")%>%sg_legend(show=TRUE, label="Department: ")
saveWidget(plot3b, file="plot3b.html")
plot3b


#plot associated graphs for visualization 4, save as HTML widgets
plot4a <- ggplot(Levels, aes(x=Year, y=freq, fill=Type))+geom_col(position="stack")+ggtitle("Engineering Faculty Composition at Cornell, 1875-1915")+ylab("Number")
plot4a <- ggplotly(plot4a)
saveWidget(plot4a, file="plot4a.html")
plot4a
plot4b<-ggplot(guests, aes(x=as.numeric(Year), y=freq, fill=guest))+theme(legend.position = "none")+geom_col(position="stack")+ggtitle("Engineering Guest Lecturers at Cornell, 1890-1895")+xlab("Year")+ylab("Number")
plot4b

#plot associated graphs for visualization 5, save as HTML widgets
plot5a <- ggplot(PROFORG, aes(x=Year, y=freq, fill=Professional_organization))+geom_col(position="stack")+ggtitle("Cornell Faculty Engineering Association Membership, 1875-1915")+ylab("Number")
plot5a <- ggplotly(plot5a)
saveWidget(plot5a, file="plot5a.html")
plot5a
plot5b<-ggplot(C_PROFS, aes(x=Year, y=freq, fill=Cornell))+geom_col(position="stack")+ggtitle("Cornell Engineering Faculty Who Graduated from Cornell, 1875-1915")+ylab("Number")
plot5b<-ggplotly(plot5b)
plot5b


               
