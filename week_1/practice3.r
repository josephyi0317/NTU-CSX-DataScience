### practice_3_question
### ptt_info_book

# ���ܼƸ�T
person.name <- c("Jiyuian", "Shawnroom", "Bigmoumou")
person.sex <- c("F", "M", "M")
person.id <- c("jiyuian520", "shawnn520", "moumou123")
person.days <- c(201, 37, 99)

# �ϥ�data.frame()�A�åH�W�z4�ӦV�q�إ�person.df
person.df <- data.frame(person.name, person.sex, person.id, person.days)

# �d��person.df���c
str(person.df)

# �d��person.df summary
summary(person.df)

# �L�Xperson.df
person.df

# �L�Xperson.df�Ĥ@�C
person.df[1, ]

# �L�Xperson.df�ĤG�C�ĤT��
person.df[2, 3]

# �ϥ�$ ���w�Xperson.df��person.id���
person.df$person.id

# �ϥ�order(), �Nperson.df$person.days�Ƨǫ�, �إ�days.position
days.postion <- order(person.df$person.days)

# �ϥ�days.postion, �Ƨ�person.df
person.df[days.postion, ]

# �ϥ�grepl()�A��Xperson.df$person.id����520�믫��
spirit.520 <- grepl("520", person.df$person.id)

# �z��X520�a�ڪ�����
person.df[spirit.520, ]

