library(stringr)
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
        for (i in 1:3){
                for (j in 1:length(foo[[i]])){
                        foo[[i]][[j]] = lapply(foo[[i]][[j]], as.numeric)
                        foo[[i]][[j]] = melt(foo[[i]][[j]])
                }
        }
        dataf[lx] = lapply(foo[[1]], mean, na.rm = TRUE)
        dataf[ly] = lapply(foo[[2]], mean, na.rm = TRUE)
        dataf[lz] = lapply(foo[[3]], mean, na.rm = TRUE)
        closeAllConnections()
        return(dataf)
}