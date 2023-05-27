# t-test
组间差异分析之t检验
#读入R中自带的ToothGrowth数据集
ToothGrowth
#数据集中包含3列信息，分别是：服用VC的种类，剂量，以及在此条件下豚鼠牙齿的长度
#先对数据分组进行重新编码
supp1 = 1*(ToothGrowth$supp == "VC")+2*(ToothGrowth$supp == "OJ")
supp = as.factor(supp1)
len = ToothGrowth$len
dose = ToothGrowth$dose
ToothGrowth2 = data.frame(len,supp,dose)
#使用T检验的前提是查看数据是否符合正态分布，因此采用Shapiro-Wilk检验，当且仅当两个P值均大于0.05，表明数据符合正态分布
shaprio = tapply(len,supp,shapiro.test)
shapiro$
shaprio$
#综上数据分布通过了正态性假设检验，即可执行t检验
#如果样本间互相独立，采用独立样本t检验，R中t.test()默认两组间相互独立
t_test =  t.test(len~supp,ToothGrowth2,alternative = 'two.sided')
t_test
t_test$p.value
#P值>0.05,接受原假设，两组间没有显著差异，即采用不同的形式摄入VC对豚鼠的牙齿长度没有1显著影响
#可视化展示
boxplot(len~supp,ToothGrowth2,col = c('green','orange'),ylab = 'len',xlab  = 'supp',main = 't_test':P-value < 0.001)
library(doBy)
library(ggplot2)
fun = function(x)
{
c(mean = mean(x),sd = sd(x))
}
dat = summaryBy(len~supp,ToothGrowth2,FUN = fun)
ggplot(dat,aes(supp,len.mean,fill = supp))+
  geom_col(width = 0.4, show.legend = FALSE) +
  geom_errorbar(aes(ymin = len.mean - len.sd, ymax = len.mean + len.sd), width = 0.15, size = 0.5) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'blue', fill = 'transparent'), plot.title = element_text(hjust = 0.5)) +
  labs(x = 'supp', y = 'len', title = 't-test: p-value < 0.001')
