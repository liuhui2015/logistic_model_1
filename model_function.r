###------函数
rm(list=ls())
library(gmodels)
library(fpc)
MY_iv<-function(x,y)
{
	z<-(x-y)*(log(x)-log(y))
	return(z)
}
MY_woe<-function(x,y)
{
	z<-log(x/y)
	return(z)
}

MY_first_process_data<-function(x,my_total)
{
	my_freq <- table(x)
	my_name_val <- as.numeric(names(my_freq))
  my_flag <- 0
  my_num <- 0
  my_first_group <- list(0) ##第一次分组
  my_group_num <- 0
  my_need_hb <-0
	for (i in 1:length(my_freq))   ###最优分段
	{
		if(my_name_val[i]==-1)
		{
			my_group_num <- my_group_num + 1
			my_first_group[[my_group_num]] <- my_name_val[i]
			next
		}
		my_flag <- my_flag+my_freq[i]
        
		if(my_flag<my_total/16)
		{
			my_num <- my_num+1
			my_need_hb[my_num] <- my_name_val[i]
			if(i==length(my_freq))
			{

				my_first_group[[my_group_num]] <- c(my_first_group[[my_group_num]],my_need_hb)
			}
		}
		else
		{
			my_num <- my_num+1
			my_need_hb[my_num] <- my_name_val[i]
			my_group_num <- my_group_num + 1
			my_first_group[[my_group_num]] <- my_need_hb
			rm(my_need_hb)
			my_need_hb <- 0
			my_flag <- 0
			my_num <- 0
		}
	}
	return(my_first_group)
}

MY_second_process_data<-function(x,myju,my_total)
{
	myclass<-myju$cluster 
	my_x <- list(0)
    ###先合并-1的值
    if(x[[1]][[1]]==-1)
    {
    	my_po<-which(myclass==myclass[1])
    	if(length(my_po)>1)  ##能找到-1的归类的情况
    	{
    		x[[my_po[2]]][[3]] <- x[[my_po[2]]][[3]]+x[[1]][[3]]
    		x[[my_po[2]]][[4]] <- x[[my_po[2]]][[4]]+x[[1]][[4]]
    		x[[my_po[2]]][[5]] <- x[[my_po[2]]][[3]]/(x[[my_po[2]]][[3]]+x[[my_po[2]]][[4]])
    		x[[my_po[2]]][[6]] <- x[[my_po[2]]][[6]]+x[[1]][[6]]
    		x[[my_po[2]]][[7]] <- x[[my_po[2]]][[7]]+x[[1]][[7]]
    		x[[my_po[2]]][[8]] <- -1
    		x<-x[-c(1)]  ###删除-1列
    		myclass<-myclass[-c(1)]
    	}
    	else
    	{
    		if(x[[1]][[3]]+x[[1]][[4]]<my_total*0.1)  ##判断-1个数太少的情况
    		{
    			my_min<-0
    			for(i in 2:length(x))
    			{
    				my_min[i-1]<-abs(x[[i]][[5]]-x[[1]][[5]])
    			}
    			my_min_po <- which(my_min==min(my_min))+1

    			x[[my_min_po[1]]][[3]] <- x[[my_min_po[1]]][[3]]+x[[1]][[3]]
  		    x[[my_min_po[1]]][[4]] <- x[[my_min_po[1]]][[4]]+x[[1]][[4]]
  		    x[[my_min_po[1]]][[5]] <- x[[my_min_po[1]]][[3]]/(x[[my_min_po[1]]][[3]]+x[[my_min_po[1]]][[4]])
  		    x[[my_min_po[1]]][[6]] <- x[[my_min_po[1]]][[6]]+x[[1]][[6]]
  		    x[[my_min_po[1]]][[7]] <- x[[my_min_po[1]]][[7]]+x[[1]][[7]]
  		    x[[my_min_po[1]]][[8]] <- -1
          x<-x[-c(1)]  ###删除-1列
          myclass<-myclass[-c(1)]
    		}
    	}
    }

    ###*****************开始分组
    my_temp <- 0
    
    my_temp[1] <- x[[1]][[1]]
		my_temp[2] <- x[[1]][[2]]
		my_temp[3] <- x[[1]][[3]]
		my_temp[4] <- x[[1]][[4]]
		my_temp[5] <- x[[1]][[5]]
		my_temp[6] <- x[[1]][[6]]
		my_temp[7] <- x[[1]][[7]]
		my_temp[8] <- x[[1]][[8]]
	
    my_po <- 0   ##记录位置

    if(length(myclass)>=2)
    {
    	for(i in 2:length(myclass))
			{
				if(myclass[i]==myclass[i-1])
				{
					my_temp[2] <- x[[i]][[2]]
					my_temp[3] <-x[[i]][[3]]+my_temp[3]
					my_temp[4] <-x[[i]][[4]]+my_temp[4]
					my_temp[5] <- my_temp[3]/(my_temp[3]+my_temp[4])
					my_temp[6] <- x[[i]][[6]]+my_temp[6]
	    		my_temp[7] <- x[[i]][[7]]+my_temp[7]
	    		my_temp[8] <- x[[i]][[8]]+my_temp[8]   #********少加一个my_temp[8]，会导致某些变量的-1值不被标示*****
				}
				else
				{
					my_po <- my_po+1
					my_x[[my_po]] <- my_temp
					my_temp[1] <- x[[i]][[1]]
			    my_temp[2] <- x[[i]][[2]]
			    my_temp[3] <- x[[i]][[3]]
			    my_temp[4] <- x[[i]][[4]]
			    my_temp[5] <- x[[i]][[5]]
			    my_temp[6] <- x[[i]][[6]]
			    my_temp[7] <- x[[i]][[7]]
			    my_temp[8] <- x[[i]][[8]] 
				}
				if(i==length(myclass))
		    {
		    	my_po <- my_po+1
					my_x[[my_po]] <- my_temp  
		    }
			}
    }
    else
    {
    	my_x[[1]] <- my_temp 
    }
	return(my_x)       
}

MY_first_bgrate_data<-function(x,y)
{
  my_first_group<-MY_first_process_data(x,length(x))
	cross.table <- CrossTable(x,y)
	my_name_val <- as.numeric(rownames(cross.table$prop.col))
  my_list_b_g <- list(0)  ##用来存储个样本的分组的好坏比
  my_bad_rating <- 0
  my_flag <- 0
  my_temp <- c(1:8)
	for(i in 1:length(my_first_group))
	{
		my_bad <- 0
		my_good <- 0
    my_length <-length(my_first_group[[i]])
    my_bad_woe <- 0
    my_good_woe <- 0
		for (j in 1:my_length) 
		{
			my_po <- which(my_name_val==my_first_group[[i]][[j]])
			my_bad <- my_bad + cross.table$t[my_po,2]
			my_good <- my_good + cross.table$t[my_po,1]
			my_bad_woe <- my_bad_woe + cross.table$prop.col[my_po,2]
      my_good_woe <- my_good_woe + cross.table$prop.col[my_po,1]
		}
		my_temp[1] <- my_first_group[[i]][[1]]
		my_temp[2] <- my_first_group[[i]][[my_length]]
		my_temp[3] <- my_bad
		my_temp[4] <- my_good
		my_temp[5] <- my_bad/(my_bad+my_good)
    my_temp[6] <- my_good_woe
    my_temp[7] <- my_bad_woe
    my_temp[8] <- 0   
		my_flag <- my_flag + 1
    my_list_b_g[[my_flag]] <- my_temp
    my_bad_rating[my_flag] <- my_temp[5]
	}
	
  
  ##--利用好坏比聚类分组
  if(length(my_bad_rating)>3)
  {
  	myju<-kmeans(my_bad_rating, 3)
  }
  else
  {
   	myju<-dbscan(my_bad_rating,0.2,MinPts=1,scale=TRUE,showplot=TRUE,method="raw")   ###另一种聚类
  }
  
  my_list <- MY_second_process_data(my_list_b_g,myju,length(x))
  
	my_double<-1  ###用来控制参数的
	while((length(my_list)!=length(my_list_b_g)&length(my_list)>1)||length(my_list)>8) ##检测是否还需要分类
	{
		my_rating<-0
		for(i in 1:length(my_list))
		{
			my_rating[i] <- my_list[[i]][[5]]
		}
		my_double<-my_double + 1
		
		myju<-dbscan(my_rating,0.2*my_double,MinPts=1,scale=TRUE,showplot=TRUE,method="raw")   ###另一种聚类
		my_list_b_g<-my_list
		my_list <- MY_second_process_data(my_list_b_g,myju,length(x))
	} 
	return(my_list)
}

###---非连续型变量的处理

MY_discrect_process_data<-function(x)  
{
	my_row<-x$prop.row
	my_clo<-x$prop.col
	my_t<-x$t
	my_names<-rownames(x$prop.col)

	y_name<-list(0)   ###存储分组的情况
	y_num<-list(0)
	my_rating<-0

	my_iv<-0
    
	myju<-dbscan(my_row[,2],0.2,MinPts=1,scale=TRUE,showplot=TRUE,method="raw")   ###另一种聚类

	my_class<-myju$cluster
  my_un_class<-unique(my_class)  ##具体分了多少类
    
	for(i in 1:length(my_un_class))
	{
		my_po<-which(my_class==my_un_class[i])
		my_temp_name<-"0"
		my_sum_bad<-0
		my_sum_good<-0
		my_bad_r<-0
		my_good_r<-0
		for(j in 1:length(my_po))
		{
			my_temp_name[j]<-my_names[my_po[j]]
			my_sum_bad<-my_sum_bad+my_t[my_po[j],2]
			my_sum_good<-my_sum_good+my_t[my_po[j],1]
			my_bad_r<-my_bad_r+my_clo[my_po[j],2]  ###---bad的woe
			my_good_r<-my_good_r+my_clo[my_po[j],1] ###---good的woe
		}
		my_rating[i]<-my_sum_bad/(my_sum_bad+my_sum_good)
		y_name[[i]]<-my_temp_name
		
		y_num[[i]] <- c(my_sum_bad,my_sum_good,my_rating[i],my_bad_r,my_good_r)
	}
	
	my_po_0<-which(my_rating<1e-6)
	my_po_1<-which(my_rating>0.9999)
	my_po<-c(my_po_0,my_po_1)  ##需要被合并的位置
	
	
	y_name_1<-y_name
	y_num_1<-y_num
	
	if(length(my_po)>0) ##表示有为0的项,删除这些项目
	{
		for(i in 1:length(my_po))
		{
			y_name_1 <- y_name_1[-c(my_po[i])]
			y_num_1 <- y_num_1[-c(my_po[i])]
		}
		
		
		for(i in 1:length(my_po))
		{
			my_min<-10  ###最小值的初值
			my_j_po<-0  ####我的相似位置
			for(j in 1:length(y_num_1))
			{
				my_diff<-abs(y_num[[my_po[i]]][[3]]-y_num_1[[j]][[3]])
				if(my_diff<my_min)
				{
					my_min<-my_diff
					my_j_po<-j
				}
			}
			y_name_1[[my_j_po]]<-c(y_name_1[[my_j_po]],y_name[[my_po[i]]])
			y_num_1[[my_j_po]][[1]]<-y_num_1[[my_j_po]][[1]]+y_num[[my_po[i]]][[1]]
			y_num_1[[my_j_po]][[2]]<-y_num_1[[my_j_po]][[2]]+y_num[[my_po[i]]][[2]]
			y_num_1[[my_j_po]][[3]]<-y_num_1[[my_j_po]][[1]]/(y_num_1[[my_j_po]][[1]]+y_num[[my_po[i]]][[2]])
			y_num_1[[my_j_po]][[4]]<-y_num_1[[my_j_po]][[4]]+y_num[[my_po[i]]][[4]]
			y_num_1[[my_j_po]][[5]]<-y_num_1[[my_j_po]][[5]]+y_num[[my_po[i]]][[5]]
			
		}
	
	}
	
  y<-list(0)
  y_temp<-list(0)
  for(i in 1:length(y_name_1))
  {
  	y_temp[[1]] <- y_name_1[[i]]
  	y_temp[[2]] <- y_num_1[[i]]
  	y[[i]] <-y_temp
  }

	return(y)
}


######-----------跳出IV值大的向量，并对其用woe映射
MY_shine<-function(x,y,my_group,my_woe,my_vari_property)  
{
	####x表示整个数据，y表示需要获得的变量列，my_group是分好的额组
	my_po<-which(y==1)+1  ###满足IV》0.02的向量
	my_data<-x[,my_po]  ##表示取对应的变量
	my_property<-my_vari_property[my_po]

	my_newdata_project<-my_data  ##
	
	for(i in 1:length(my_property))   
	{
	    my_newdata_project[,i]<-as.numeric(my_newdata_project[,i])  ###转变属性
			if(my_property[i]==1)    ###表示非连续变量
			{
				x_temp<-as.character(my_data[,i])
				x_flag<-c(1:dim(my_data)[1])*0  ###获取位置时的标志位
				
				for(j in 1:length(my_group[[i]]))
				{
					for(k in 1:length(my_group[[i]][[j]]))
					{
						my_position<-which(x_temp==my_group[[i]][[j]][[k]])
						if(length(my_position)!=0)
						{
							my_newdata_project[my_position,i]<-as.numeric(my_woe[[i]][[j]])
							x_flag[my_position]<-1
						}
					}
				}
				
				my_no_in_group_po<-which(x_flag==0)  ###找到没有在组群里的值，并赋值为最小值
				if(length(my_no_in_group_po)!=0)
				{
					my_newdata_project[my_no_in_group_po,i]<-min(as.numeric(my_woe[[i]]))
				}
			}
			else
			{
				x_temp<-as.numeric(my_data[,i])
				
				x_flag<-c(1:dim(my_data)[1])*0  ###获取位置时的标志位
				
				for(j in 1:length(my_group[[i]]))
				{
					if(length(my_group[[i]][[j]])==3)  ###表示肯定有-1
					{
					  #####将最大值变为Inf
						my_L1<-length(my_group[[i]])
	       		my_L2<-length(my_group[[i]][[my_L1]])
	       		my_group[[i]][[my_L1]][[my_L2]]<-Inf
					  
						y_temp<-sort(as.numeric(my_group[[i]][[j]]))
						my_position<-which(x_temp==-1)
						if(length(my_position)!=0)
						{
							my_newdata_project[my_position,i]<-as.numeric(my_woe[[i]][[j]])
							x_flag[my_position]<-1
						}
						
						my_position_1<-which(x_temp>=y_temp[2]&x_temp<=y_temp[3])
						if(length(my_position_1)!=0)
						{
							my_newdata_project[my_position_1,i]<-as.numeric(my_woe[[i]][[j]])
							x_flag[my_position_1]<-1
						}
						
					}
					else if(length(my_group[[i]][[j]])==1)   ##只有1的情况
					{
					 
						my_position<-which(x_temp==as.numeric(my_group[[i]][[j]][[1]]))
						if(length(my_position)!=0)
						{
							my_newdata_project[my_position,i]<-as.numeric(my_woe[[i]][[j]])
							x_flag[my_position]<-1
						}
					}
					else
					{
					  ####将最大值变为Inf
					  my_L1<-length(my_group[[i]])
	       		my_L2<-length(my_group[[i]][[my_L1]])
	       		my_group[[i]][[my_L1]][[my_L2]]<-Inf
	       		
	       		
						my_position_1<-which(x_temp>=as.numeric(my_group[[i]][[j]][[1]])&x_temp<=as.numeric(my_group[[i]][[j]][[2]]))
						if(length(my_position_1)!=0)
						{
							my_newdata_project[my_position_1,i]<-as.numeric(my_woe[[i]][[j]])
							x_flag[my_position_1]<-1
						}
					}
					
				}
				
				my_no_in_group_po<-which(x_flag==0)  ###找到没有在组群里的值，并赋值为最小值
				
				if(length(my_no_in_group_po)!=0)
				{
					my_newdata_project[my_no_in_group_po,i]<-min(as.numeric(my_woe[[i]]))
				}
				
			}
	}

	return(my_newdata_project)
}

####相关检验
MY_detect_cor<-function(x)
{

	###x表示输入的数据集
	my_every_cor<-list(0)
	my_strong_cor_var<-0   ##记录下所有的强相关变量，需要删除掉的坐标
	my_nums<-0
	for(i in 1:(dim(x)[2]-1))
	{
		my_strong_cor<-0   ##强相关
		my_weak_cor<-0   ##弱相关
		strong_num<-0
		weak_num<-0
		for(j in (i+1):dim(x)[2])
		{
			my_cor_temp<-cor(as.numeric(x[,i]),as.numeric(x[,j]))

			if(is.na(my_cor_temp)==FALSE&abs(my_cor_temp)>0.4)  ##弱相关
			{
				if(abs(my_cor_temp)>0.8)
				{
					strong_num <-   strong_num+1
				    my_strong_cor[strong_num] <- j

				    my_nums<-my_nums+1
				    my_strong_cor_var[my_nums]<-j
				}
				else
				{
					weak_num <- weak_num+1
					my_weak_cor[weak_num] <- j
				}
				
			}
		}
		
		my_every_cor[[i]]<-list(my_strong_cor,my_weak_cor)
	}
	my_strong_var_posi<-unique(my_strong_cor_var)   ##获得相关性强的变量位置
	return(my_strong_var_posi)
}

MY_write_file<-function(my_group,my_woe,my_name,my_file)
{
	for(i in 1:length(my_name))
	{
		write.table(my_name[i], file=my_file, row.names=FALSE, col.names=FALSE, append=TRUE, sep=",") 
        
        for(j in 1:length(my_group[[i]]))
        {
        	my_temp<-matrix(,nrow=1,ncol=length(my_group[[i]][[j]])+1)
        	for(k in 1:length(my_group[[i]][[j]]))
        	{
        		my_temp[k]<-my_group[[i]][[j]][[k]]
        	}
        	my_temp[length(my_group[[i]][[j]])+1]<-my_woe[[i]][[j]]
        	write.table(my_temp, file=my_file, row.names=FALSE, col.names=FALSE, append=TRUE, sep=",") 
        }
	}

}

###----数据初始化
MY_init_data<-function(x)
{
	###----将空值和缺省以及极值改为-1
	for(i in 1:(dim(x)[2]))
	{
		x_temp<-x[,i]
		x_temp[is.na(x_temp)]<--1
		x_temp[is.null(x_temp)]<--1
		my_n<-as.numeric(x_temp)
		my_po_1<-which(my_n<(-1))
		x_temp[my_po_1]<--1
		x[,i]<-x_temp
	}

		my_po<-which(x[,69]!=0)
		x[my_po,70]<-25
		
		my_po<-which(x[,69]<0)
		x[my_po,70]<-25
	
	###将有些变量改为非连续型的变量
	x[,68] <- as.character(x[,68])
	for(i in 71:74)
	{
		x[,i] <- as.character(x[,i])
	}
	y<-x
	return(y)
}

####---获取变量的得分
MY_logistic<-function(x,x_1)
{
	 ##---x表示映射的WOE值，x_1表示系数
   y<-((-28.85)*x_1*x)  ##481.89-28.85*(my_coef[1]+my_coef[i]*x)
   return(y)
}
