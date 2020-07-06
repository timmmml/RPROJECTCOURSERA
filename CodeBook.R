library(stringr)
#the below function gets data from file_name, X_train.txt or X_test.txt, and 
#converts the txt information into an R data frame that contains the means and 
#standard deviations. After that, the function reads information from file_name2
#, assumed to be either y_train.txt or y_test.txt and pastes the result ids to 
#the corresponding mean & std_dev. Then, the function takes the file_name3, 
#assumed to be either subject_train.txt or subject_test.txt and pastes the 
#subject id for each observation. Lastly, the function adds a result_label 
#variable that matches the result ids for worded labels. 
get_training_data = function(file_name, file_name2, file_name3, place_to_put){
        con = file(file_name)
        foo = readLines(con)
        newdf = data.frame(mean = 1:length(foo), standard_dev = 1:length(foo))
        for (i in 1:length(foo)){
                fool = foo[i]
                bar = str_trim(fool)
                bar = strsplit(bar, ' ')[[1]]
                bar = as.numeric(bar)
                newdf$mean[i] = mean(bar, na.rm = TRUE)
                newdf$standard_dev[i] = var(bar, na.rm = TRUE)^0.5
        }
        close(con)
        con = file(file_name2)
        con2 = file(file_name3)
        foo = readLines(file_name2)
        foo1 = readLines(file_name3)
        for (i in 1:length(foo)){
                newdf$result_id[i] = foo[i]
                newdf$person_id[i] = foo1[i]
        }
        newdf$result_label = factor(newdf$result_id, levels = c(1,2,3,4,5,6), labels = c('WALKING', 'WALKING_UPSTAIRS', 'WALKING_DOWNSTAIRS', 'SITTING', 'STANDING', 'LAYING'))
        newdf = rbind(place_to_put, newdf)
        closeAllConnections()
        return(newdf)
}

#The below function takes one variable in the Inertial Signals folder (total
#acceleration, body gyro, or body acceleration), and puts its mean for each 
#observation down in a data frame. The data frame is then c-binded to the pre-
#existing data frame for an other variable passed in the args. 
load_body_data = function(xdata, ydata, zdata, label, dataf){
        lx = paste(label, 'x', sep = '')
        ly = paste(label, 'y', sep = '')
        lz = paste(label, 'z', sep = '')
        conx = file(xdata)
        cony = file(ydata)
        conz = file(zdata)
        foo = lapply(list(conx, cony, conz), readLines)
        foo = lapply(foo,str_trim) 
        foo = lapply(foo, strsplit, ' ')
        tempdataff = data.frame(lx = 1:length(foo[[1]]), ly = 1:length(foo[[1]]), lz = 1:length(foo[[1]]))
        names(tempdataff) = c(lx, ly, lz)
        for (i in 1:3){
                for (j in 1:length(foo[[i]])){
                        foo[[i]][[j]] = lapply(foo[[i]][[j]], as.numeric)
                        foo[[i]][[j]] = melt(foo[[i]][[j]], na.rm = TRUE)
                        foo[[i]][[j]] = foo[[i]][[j]][1:(length(foo[[i]][[j]])/2)]
                        tempdataff[j, i] = mean(foo[[i]][[j]]$value)
                }
        }
        dataf = cbind(dataf, tempdataff)
        closeAllConnections()
        return(dataf)
}
dataff = data.frame(accelx = 1:7352)
dataff = load_body_data('total_acc_x_train.txt', 'total_acc_y_train.txt', 'total_acc_z_train.txt', 'accel', dataff)
dataff = load_body_data('body_gyro_x_train.txt', 'body_gyro_y_train.txt', 'body_gyro_z_train.txt', 'gyro', dataff)
dataff = load_body_data('body_acc_x_train.txt', 'body_acc_y_train.txt', 'body_acc_z_train.txt', 'bodyaccel', dataff)
setwd("~/R/RPROJECTCOURSERA/UCI HAR Dataset/test/Inertial Signals")
datafftest = data.frame(accelx = 1:2947)
datafftest = load_body_data('total_acc_x_test.txt', 'total_acc_y_test.txt', 'total_acc_z_test.txt', 'accel', datafftest)
datafftest = load_body_data('body_gyro_x_test.txt', 'body_gyro_y_test.txt', 'body_gyro_z_test.txt', 'gyro', datafftest)
datafftest = load_body_data('body_acc_x_test.txt', 'body_acc_y_test.txt', 'body_acc_z_test.txt', 'bodyaccel', datafftest)
dataff = rbind(dataff, datafftest)
#the below line binds the training set and the testing set. 
bigdata = cbind(newdf, dataff)
bigdata$person_id = sapply(bigdata$person_id, as.numeric)
bigdata$person_id = factor(bigdata$person_id)
bigdata$result_label = factor(bigdata$result_label)
#the below two lines sort the data into by_activity and by_person. 
by_activity = summarise(group_by(bigdata, bigdata$result_label), mean_total_accelx = mean(accelx), mean_total_accely = mean(accely), mean_total_accelz = mean(accelz), mean_gyrox = mean(gyrox), mean_gyroy = mean(gyroy), mean_gyroz = mean(gyroz), mean_body_accelx = mean(bodyaccelx), mean_body_accely = mean(bodyaccely), mean_body_accelz = mean(bodyaccelz))
by_person = summarise(group_by(bigdata, bigdata$person_id), mean_total_accelx = mean(accelx), mean_total_accely = mean(accely), mean_total_accelz = mean(accelz), mean_gyrox = mean(gyrox), mean_gyroy = mean(gyroy), mean_gyroz = mean(gyroz), mean_body_accelx = mean(bodyaccelx), mean_body_accely = mean(bodyaccely), mean_body_accelz = mean(bodyaccelz))