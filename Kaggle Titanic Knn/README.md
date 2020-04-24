Kaggle Titanic compete with Knn
================

## Knn

이번에 해볼 것은 캐글에서 유명한 타이타닉 데이터셋을 가지고 Knn을 활용하여 사망자 예측을 해볼 것이다.  
먼저 데이터셋과 필요한 라이브러리를 준비해보자

``` r
setwd("C:/Users/Seunggi/Downloads")
library(class)
library(dplyr)
test1 <- read.csv("test.csv")
train1 <- read.csv("train.csv")
merge1 <- bind_rows(train1, test1)
```

Kaggle에서 제공해주는 타이타닉 데이터셋은 트레이닝셋과 테스트셋이 존재한다. 테스트셋에 Survied컬럼이 없다는 걸 제외하면
나머지 컬럼은 모두 동일하고, 원활한 전처리과정을 위하여 데이터셋을 Merge하여 전처리를 시도하겠다.

## Check missing value and Preprocessing

Merge한 데이터셋에 결측값를 확인 하는 과정을 진행하여 전처리를 진행해야 한다.

``` r
head(merge1)
```

``` r
str(merge1)
```

    ## 'data.frame':    1309 obs. of  12 variables:
    ##  $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
    ##  $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
    ##  $ Name       : chr  "Braund, Mr. Owen Harris" "Cumings, Mrs. John Bradley (Florence Briggs Thayer)" "Heikkinen, Miss. Laina" "Futrelle, Mrs. Jacques Heath (Lily May Peel)" ...
    ##  $ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
    ##  $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
    ##  $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
    ##  $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
    ##  $ Ticket     : chr  "A/5 21171" "PC 17599" "STON/O2. 3101282" "113803" ...
    ##  $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
    ##  $ Cabin      : chr  "" "C85" "" "C123" ...
    ##  $ Embarked   : chr  "S" "C" "S" "S" ...

``` r
colSums(is.na(merge1))
```

    ## PassengerId    Survived      Pclass        Name         Sex         Age 
    ##           0         418           0           0           0         263 
    ##       SibSp       Parch      Ticket        Fare       Cabin    Embarked 
    ##           0           0           0           1           0           0

확인 결과 테스트셋이 합쳐져서 생긴 Survied컬럼 결측값과 Age, Fare값에 결측값이 존재하는 것을 확인 할 수 있다.

``` r
str(merge1)
```

    ## 'data.frame':    1309 obs. of  12 variables:
    ##  $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
    ##  $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
    ##  $ Name       : chr  "Braund, Mr. Owen Harris" "Cumings, Mrs. John Bradley (Florence Briggs Thayer)" "Heikkinen, Miss. Laina" "Futrelle, Mrs. Jacques Heath (Lily May Peel)" ...
    ##  $ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
    ##  $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
    ##  $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
    ##  $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
    ##  $ Ticket     : chr  "A/5 21171" "PC 17599" "STON/O2. 3101282" "113803" ...
    ##  $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
    ##  $ Cabin      : chr  "" "C85" "" "C123" ...
    ##  $ Embarked   : chr  "S" "C" "S" "S" ...

``` r
merge1$Sex <- gsub('female','1',merge1$Sex)
merge1$Sex <- gsub('male','0',merge1$Sex)
merge1$Sex <- as.numeric(merge1$Sex)
missingage <- merge1[which(is.na(merge1$Age)),]
agetrain <- merge1[which(!is.na(merge1$Age)),]
model <- lm(Age ~ Pclass+Sex, data = agetrain)
pred <- predict(model, missingage)
pred <- as.data.frame(pred)
count <- 0
for (i in which(is.na(merge1$Age))) {
  count <- count + 1
  merge1$Age[i] <- pred$pred[count]
}
colSums(is.na(merge1))
```

    ## PassengerId    Survived      Pclass        Name         Sex         Age 
    ##           0         418           0           0           0           0 
    ##       SibSp       Parch      Ticket        Fare       Cabin    Embarked 
    ##           0           0           0           1           0           0

위 과정은 성별 컬럼을 트레이닝을 위해 0,1로 전처리 해주고, age컬럼에 많은 수의 결측값이 존재하기 떄문에 회귀분석을 통해
예측값으로 결측값이 존재하는 컬럼에 채워주는 과정이다.  
결과를 확인해 보면 age컬럼에 존재하던 결측 값들이 사라진 것을 확인 할 수 있다. 물론 결측값이 였기 때문에 정확한 나이가
컬럼에 채워졌다고 생각 할 수 없지만, Pclass컬럼과 Sex컬럼을 이용해 회귀분석을 통해 예측값을 계산 했기 때문에
비교적 비슷한 값을 유추해 넣었다고 생각할 수 있겠다.

``` r
merge1$Fare[merge1$Fare==0] <- NA
merge1$Embarked[which(merge1$Embarked=='')] <- 'C'
colSums(is.na(merge1))
```

    ## PassengerId    Survived      Pclass        Name         Sex         Age 
    ##           0         418           0           0           0           0 
    ##       SibSp       Parch      Ticket        Fare       Cabin    Embarked 
    ##           0           0           0          18           0           0

``` r
merge1$Fare[which(is.na(merge1$Fare))] <- median(merge1$Fare, na.rm = T)
colSums(is.na(merge1))
```

    ## PassengerId    Survived      Pclass        Name         Sex         Age 
    ##           0         418           0           0           0           0 
    ##       SibSp       Parch      Ticket        Fare       Cabin    Embarked 
    ##           0           0           0           0           0           0

위 과정에서는 Fare컬럼에 있는 0값을 모두 결측 값으로 만든뒤, 결측값을 제외한 Fare컬럼 값들의 중간값으로 결측값을 처리
해주는 과정이다.  
다음으로 Embarked컬럼에도 빈 값(" ")이 존재하는데 이 값을 가진 컬럼을 Embarked컬럼 값중 C로 채워준다. 물론
다른 Embarked 값인 S나 Q를 채워줘도 무방하다.

## Knn training

이제 결측 값에 대한 전처리 과정을 끝내고 Knn학습을 위한 데이터 프레임을 만들고 테스트를 진행 하겠다.

``` r
df <- merge1[-c(1,4,9,11)]
```

``` r
str(df)
```

    ## 'data.frame':    1309 obs. of  8 variables:
    ##  $ Survived: int  0 1 1 1 0 0 0 0 1 1 ...
    ##  $ Pclass  : int  3 1 3 1 3 3 1 3 3 2 ...
    ##  $ Sex     : num  0 1 1 1 0 0 0 0 1 1 ...
    ##  $ Age     : num  22 38 26 35 35 ...
    ##  $ SibSp   : int  1 1 0 1 0 0 0 3 0 1 ...
    ##  $ Parch   : int  0 0 0 0 0 0 0 1 2 0 ...
    ##  $ Fare    : num  7.25 71.28 7.92 53.1 8.05 ...
    ##  $ Embarked: chr  "S" "C" "S" "S" ...

``` r
df$Survived <- as.factor(df$Survived)
df$Embarked[which(df$Embarked=='C')] <- 0
df$Embarked[which(df$Embarked=='Q')] <- 1
df$Embarked[which(df$Embarked=='S')] <- 2
df$Embarked <- as.numeric(df$Embarked)

```

먼저 학습에 연관이 없을 것 같은 1번 4번 9번 11번 컬럼을 제외하고 df프레임을 새로 생성한다. Knn학습을 위해 타겟이
되는 Survived 컬럼을 factor로 전환 시켜주고, Embarked컬럼 값들을 numeric요소로 전환시켜준다.

``` r
train_target <- df[1:891,1]
normalize <- function(x){return( (x-min(x))/(max(x)-min(x)) )}
df <- as.data.frame(lapply(df[,c(2:8)],normalize))
dt.ts <- df[892:1309,]
dt.tr <- df[1:891,]
k <- as.integer(sqrt(891))
model <- knn(train = dt.tr,
             test = dt.ts,    
             cl = train_target,
             k=k )
model
```

    ##   [1] 0 0 0 0 0 0 1 0 1 0 0 0 1 0 1 1 0 0 0 1 0 0 1 0 1 0 1 0 0 0 0 0 0 0 0 0 0
    ##  [38] 0 0 0 0 0 0 1 1 0 0 0 1 0 0 0 1 1 0 0 0 0 0 1 0 0 0 1 0 1 1 0 0 1 1 0 0 0
    ##  [75] 1 0 0 1 0 1 0 0 0 0 0 0 1 0 1 0 0 0 1 0 0 0 1 0 1 0 1 0 0 0 1 0 0 0 0 0 0
    ## [112] 1 1 1 1 0 0 0 0 1 1 0 1 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0
    ## [149] 0 0 1 0 0 0 0 0 1 1 0 0 1 0 1 0 0 1 0 0 1 0 0 0 0 0 0 1 1 0 1 1 0 0 1 0 1
    ## [186] 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 1 0 0 1 0 1 0 0 0 0 1 0 0 1 0 1 0 1 0
    ## [223] 1 0 1 1 0 1 0 0 0 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 1 0 1 1 1 0 0 0 0 0 0 0 1
    ## [260] 0 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 1 0 0 0 0 1 0 1 1 0 0 0 0 0 0 0 1 0 0 0 0
    ## [297] 1 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 1 0 1 0 0 0 1 0 0
    ## [334] 1 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1 0 1 0 0 1 1 0
    ## [371] 0 1 0 0 1 1 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 0 0
    ## [408] 0 1 0 1 1 0 0 1 0 0 0
    ## Levels: 0 1

``` r
resultdata <- data.frame(PassengerId=test1$PassengerId, Survived=model)
write.csv(resultdata, "result_submission.csv", row.names = F)
```

마지막으로 학습 대상이 되는 트레이닝셋의 Survived 컬럼을 target으로 생성한뒤, 표준화 함수를 이용하여 2번부터 8번
컬럼까지 표준화를 진행해준다.  
테스트셋과, 트레이닝셋을 구분해서 저장해둔 다음 트레이닝셋의 rownum을 제곱근하여 K를 정하고 Knn함수를 이용해서 학습
모델을 생성한다.  
생성된 모델과 테스트셋과 predict하며 정확도를 계산하여야 하는데 Kaggle 문제 자체가 지정된 양식대로 만들어 제출하면
정확도를 알려주는 방식이다.

## Result

![](C:/Users/Seunggi/Desktop/result.JPG)

위 사진은 완성된 결과csv파일을 Kaggle에 업로드 하여 얻어낸 정확도 이다. 비교적 나쁘지 않은 수준의 결과를 얻어낸 것을
확인 할 수 있고, 이번 테스트를 진행하면서 결측값등 데이터 전처리 과정이 모델을 만들에 큰 영향을 미치는 것을 확인 할 수
있었다.  
Kaggle 사이트에 더욱 많은 compete와 데이터셋을 얻을 수 있으므로 관심있는 사람이면 참고해보면 좋을 것 같다.  
visit here www.Kaggle.com
