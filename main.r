####----主程序代码main

###*************1、运行自编函数**************
source("F:\\my_data\\R数据\\R_file\\model_function.r")


####**************主函数**********************

###-----导入数据
mydata <- read.table("sample_20150521.csv",header=TRUE,sep=",")

##---数据初始化
mydata<-MY_init_data(mydata)

####-----开始计算WOE和IV
my_time<-Sys.Date()

##------file names
my_file_all_woe<-paste("all_woe_", my_time,".csv", sep = "",collapse=" ")
my_file_group_woe<-paste("my_group_woe_", my_time,".csv", sep = "",collapse=" ")
my_coef_model<-paste("my_model_coef_", my_time,".csv", sep = "",collapse=" ")
my_score_card<-paste("my_score_card_", my_time,".csv", sep = "",collapse=" ")

my_vari_property<-0  ###保存变量的属性，0为连续型，1为非连续性
myname<-colnames(mydata)
my_na_name<-"0"
my_flag_na<-0
my_keep_po<-c(1:(dim(mydata)[2]-2))*0  ####保留位置的初始化

my_list_group<-list(0)
my_list_woe<-list(0)
my_fact_num<-0 ###实际的保留group数目

for(i in 2:(dim(mydata)[2]-1))   #(dim(mydata)[2]-1)
{
	if(is.numeric(mydata[,i])==TRUE)
	{
		my_vari_property[i-1]<-0
	}
	else
	{
		my_vari_property[i-1]<-1
	}

	if(length(which(mydata[,i]==-1))/length(mydata[,i])>0.3)    ##判断缺省值（以-1表示缺省）过多时，不进行运算
	{
		my_flag_na<-my_flag_na+1
		my_na_name[my_flag_na]<-myname[i]
		next
	}
	my_cross.table <- CrossTable(mydata[,i],mydata[,dim(mydata)[2]])
	write.table(myname[i], file=my_file_all_woe, row.names=FALSE, col.names=FALSE, append=TRUE, sep=",") 
	
	if(dim(my_cross.table$prop.row)[1]>4&my_vari_property[i-1]==0)   ###保证是连续型变量  
  {
    	
		my_first_BG<-MY_first_bgrate_data(mydata[,i],mydata[,dim(mydata)[2]])
		my_lx_iv<-0
		my_woe_is_good<-0 ##判断woe是否单调
		my_list_group_temp <- list(0)
    my_list_woe_temp<-list(0)
		for(j in 1:length(my_first_BG))
		{
			my_temp<-matrix(,nrow=1,ncol=4)		
			my_temp[1,1]<-my_first_BG[[j]][[1]]
			my_temp[1,2]<-my_first_BG[[j]][[2]]
			my_temp[1,3]<-MY_woe(my_first_BG[[j]][[6]],my_first_BG[[j]][[7]])
			my_temp[1,4]<-my_first_BG[[j]][[8]]
			my_woe_is_good[j] <- my_temp[1,3]
				
			my_lx_iv<-my_lx_iv+MY_iv(my_first_BG[[j]][[6]],my_first_BG[[j]][[7]])
			write.table(my_temp, file=my_file_all_woe, row.names=FALSE, col.names=FALSE, append=TRUE, sep=",") 
	    my_group_temp<-0
	    my_group_temp[1]<-my_temp[1,1]
	    my_group_temp[2]<-my_temp[1,2]
	    if(my_first_BG[[j]][[8]]==-1)
	    {
	    	my_group_temp[3]<-my_first_BG[[j]][[8]]
	    }
			my_list_group_temp[[j]]<-my_group_temp
			my_list_woe_temp[[j]] <- my_temp[1,3]
		}

			write.table(my_lx_iv, file=my_file_all_woe, row.names=FALSE, col.names=FALSE, append=TRUE, sep=",") 
    if(my_lx_iv>0.02&length(my_woe_is_good)>1)
    {
    	my_mull<-c(1:length(my_woe_is_good))*0
    	if(length(my_woe_is_good)>3)
    	{
    		po_1<-which(my_woe_is_good>0)
    		po_2<-which(my_woe_is_good<0)
    		my_mull[po_1]<-1
    		my_mull[po_2]<--1
    		my_score<-0
    		my_com<-my_woe_is_good[1]  ###这里是判断woe是否具有单调性
    		for(i_temp in 2:length(my_woe_is_good))
    		{
    			if(my_mull[i_temp]==my_com)
    			{
    				next
    			}
    			else
    			{
    				my_score<-my_score+1
    				my_com<-my_mull[i_temp]
    			}
    		}

    		if(my_score<2)  ###表示具有单调性/length(my_woe_is_good)
    		{
    			my_keep_po[i-1]<-1
    			my_fact_num<-my_fact_num+1
    			my_list_group[[my_fact_num]]<-my_list_group_temp
    			my_list_woe[[my_fact_num]]<-my_list_woe_temp
    		}
    	}
    	else
    	{
    		my_keep_po[i-1]<-1
    		my_fact_num<-my_fact_num+1
    		my_list_group[[my_fact_num]]<-my_list_group_temp
    		my_list_woe[[my_fact_num]]<-my_list_woe_temp
    	}
    }
  }
  else  ###非连续型变量和变量较少的连续型变量
  {
  	
  	my_discrect_woe<-MY_discrect_process_data(my_cross.table)
  	
    my_iv_1<-0
    my_list_group_temp<-list(0)
    my_list_woe_temp<-list(0)
  	for(my_i in 1:length(my_discrect_woe))
  	{
  		my_temp_1<-matrix(,nrow=1,ncol=length(my_discrect_woe[[my_i]][[1]])+1)
  		for(my_j in 1:length(my_discrect_woe[[my_i]][[1]]))
  		{
  			my_temp_1[my_j]<-my_discrect_woe[[my_i]][[1]][[my_j]]
  		}
      my_list_group_temp[[my_i]]<-my_temp_1[1:length(my_discrect_woe[[my_i]][[1]])]
  		my_temp_1[my_j+1] <- MY_woe(my_discrect_woe[[my_i]][[2]][[5]],my_discrect_woe[[my_i]][[2]][[4]])
  		my_list_woe_temp[[my_i]]<-my_temp_1[my_j+1]
  		write.table(my_temp_1, file=my_file_all_woe, row.names=FALSE, col.names=FALSE, append=TRUE, sep=",") 
  		my_iv_1 <- my_iv_1 + MY_iv(my_discrect_woe[[my_i]][[2]][[5]],my_discrect_woe[[my_i]][[2]][[4]])
  	}
  	write.table(my_iv_1, file=my_file_all_woe, row.names=FALSE, col.names=FALSE, append=TRUE, sep=",") 
  	if(my_iv_1>0.02)
      {
      	my_keep_po[i-1]<-1
      	my_fact_num<-my_fact_num+1
      	my_list_group[[my_fact_num]]<-my_list_group_temp
        my_list_woe[[my_fact_num]]<-my_list_woe_temp

      }
  }
    
	
}



#######开始将IV大的挑出来，并进行映射
my_new_data<-MY_shine(mydata,my_keep_po,my_list_group,my_list_woe,my_vari_property) 


####做相关性检测
my_strong_cor_po<-MY_detect_cor(my_new_data)

###过滤掉相关性强的变量，得到新的变量样本、变量
my_position<-c(1:dim(my_new_data)[2])
my_nocor_position<-my_position[-c(my_strong_cor_po)]

my_nocor_data<-my_new_data[,c(my_nocor_position)]
my_nocor_group<-my_list_group[c(my_nocor_position)]
my_nocor_woe<-my_list_woe[c(my_nocor_position)]
my_nocor_name<-names(my_new_data)[c(my_nocor_position)]

####我的入模型变量

MY_write_file(my_nocor_group,my_nocor_woe,my_nocor_name,my_file_group_woe)

########-----用来保存model的位置
mysample_position<-rownames(mysample)
save(mysample_position,file="mysample_position_1.0")

###---训练样本 建立模型

my_nocor_data$y<-mydata$y
mysample<-my_nocor_data[sample(1:nrow(my_nocor_data),845,replace=FALSE),]


for(i in 1:(dim(mysample)[2]-1))
{
	mysample[,i]<-as.numeric(mysample[,i])
	names(mysample)[i]<-paste("V", i, sep = "",collapse=" ")
}

fit.full <- glm(y~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31,data=mysample,family=binomial())    ##logistic回归模型  familily=binomial,
reduced.model <- step(fit.full, direction="backward")  ###both backward both
summary(reduced.model)

fit.reduce <- glm(y~V1+V4+V7+V10+V11+V12+V13+V15+V18+V20+V22+V25,data=mysample,family=binomial())    ##logistic回归模型  familily=binomial,
summary(fit.reduce)


fit.reduce_2 <- glm(y~V1+V4+V7+V10+V11+V12+V13+V15+V18+V20+V22+V25,data=mysample,family=binomial())    ##logistic回归模型  familily=binomial,
summary(fit.reduce_2)

save(fit.reduce_2,file="fit.reduce1.1")


####----获得系数
my_coef<-fit.reduce_2$coefficients

my_all_coef_p<-summary(fit.reduce_2)$coefficients
write.table(my_all_coef_p, file=my_coef_model, row.names=TRUE, col.names=FALSE, append=TRUE, sep=",") 


###

my_flag_2<-1  #####----注意是从1开始，避开常数项
for(i in 1:length(my_nocor_group))
{
	my_temp_name<-paste("V", i, sep = "",collapse=" ")
	if(length(which(names(my_coef)==my_temp_name))==0)
	{
        next
	}
  my_flag_2<-my_flag_2+1  ###计数
	
	write.table(my_nocor_name[i], file=my_score_card, row.names=FALSE, col.names=FALSE, append=TRUE, sep=",")
	for(j in 1:length(my_nocor_group[[i]]))
	{
		my_temp_score<-matrix(0,nrow=1,ncol=length(my_nocor_group[[i]][[j]])+1)
		my_temp_jishu<-0
		for(k in 1:length(my_nocor_group[[i]][[j]]))
		{
			my_temp_jishu<-my_temp_jishu+1
			my_temp_score[1,my_temp_jishu]<-my_nocor_group[[i]][[j]][[k]]
		}
		my_temp_jishu<-my_temp_jishu+1
		my_temp_score[1,my_temp_jishu]<-MY_logistic(as.numeric(my_nocor_woe[[i]][[j]]),my_coef[my_flag_2])
        
		write.table(my_temp_score, file=my_score_card, row.names=FALSE, col.names=FALSE, append=TRUE, sep=",")
		rm(my_temp_score)
	}

}



