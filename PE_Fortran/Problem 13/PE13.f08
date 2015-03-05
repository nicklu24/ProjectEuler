program PE13
implicit none
real :: start, finish


call cpu_time(start)
call ans()
call cpu_time(finish)

print '("Elapsed: ", F15.6, " seconds")', finish - start


contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
        subroutine ans()
        implicit none
        real*16, dimension(100) :: array=0, arraydec=0
        Real*16 :: sigma=0, sigmadec=0
        integer :: row
        real*16 :: justadd=0, fullnum, sumfullnum=0
        

        !Added a decimal near the end of the numbers because the real array type
        !can't store 50 digit numbers.  It makes around 38 or so digits so I
        !added the decimal in a place where I knew it would fit into the data
        !type. In the array below the decimal values just go to zero because the
        !decimal is at the limit of the data type.  This still isn't returning
        !the accuraccy that we need.  So, copying the array and making a second
        !one we can look at only the decimal values.  At the end we can add the
        !sums

        !Added justadd because the "elegenant" part didnt work for some reason

        array = (/&
                37107287533902102798797998220837590246.510135740250,&
                46376937677490009712648124896970078050.417018260538,&
                74324986199524741059474233309513058123.726617309629,&
                91942213363574161572522430563301811072.406154908250,&
                23067588207539346171171980310421047513.778063246676,&
                89261670696623633820136378418383684178.734361726757,&
                28112879812849979408065481931592621691.275889832738,&
                44274228917432520321923589422876796487.670272189318,&
                47451445736001306439091167216856844588.711603153276,&
                70386486105843025439939619828917593665.686757934951,&
                62176457141856560629502157223196586755.079324193331,&
                64906352462741904929101432445813822663.347944758178,&
                92575867718337217661963751590579239728.245598838407,&
                58203565325359399008402633568948830189.458628227828,&
                80181199384826282014278194139940567587.151170094390,&
                35398664372827112653829987240784473053.190104293586,&
                86515506006295864861532075273371959191.420517255829,&
                71693888707715466499115593487603532921.714970056938,&
                54370070576826684624621495650076471787.294438377604,&
                53282654108756828443191190634694037855.217779295145,&
                36123272525000296071075082563815656710.885258350721,&
                45876576172410976447339110607218265236.877223636045,&
                17423706905851860660448207621209813287.860733969412,&
                81142660418086830619328460811191061556.940512689692,&
                51934325451728388641918047049293215058.642563049483,&
                62467221648435076201727918039944693004.732956340691,&
                15732444386908125794514089057706229429.197107928209,&
                55037687525678773091862540744969844508.330393682126,&
                18336384825330154686196124348767681297.534375946515,&
                80386287592878490201521685554828717201.219257766954,&
                78182833757993103614740356856449095527.097864797581,&
                16726320100436897842553539920931837441.497806860984,&
                48403098129077791799088218795327364475.675590848030,&
                87086987551392711854517078544161852424.320693150332,&
                59959406895756536782107074926966537676.326235447210,&
                69793950679652694742597709739166693763.042633987085,&
                41052684708299085211399427365734116182.760315001271,&
                65378607361501080857009149939512557028.198746004375,&
                35829035317434717326932123578154982629.742552737307,&
                94953759765105305946966067683156574377.167401875275,&
                88902802571733229619176668713819931811.048770190271,&
                25267680276078003013678680992525463401.061632866526,&
                36270218540497705585629946580636237993.140746255962,&
                24074486908231174977792365466257246923.322810917141,&
                23053081172816430487623791969842487255.036638784583,&
                91430288197103288597806669760892938638.285025333403,&
                34413065578016127815921815005561868836.468420090470,&
                11487696932154902810424020138335124462.181441773470,&
                63783299490636259666498587618221225225.512486764533,&
                67720186971698544312419572409913959008.952310058822,&
                95548255300263520781532296796249481641.953868218774,&
                76085327132285723110424803456124867697.064507995236,&
                37774242535411291684276865538926205024.910326572967,&
                23701913275725675285653248258265463092.207058596522,&
                29798860272258331913126375147341994889.534765745501,&
                18495701454879288984856827726077713721.403798879715,&
                38298203783031473527721580348144513491.373226651381,&
                34829543829199918180278916522431027392.251122869539,&
                40957953066405232632538044100059654939.159879593635,&
                29746152185502371307642255121183693803.580388584903,&
                41698116222072977186158236678424689157.993532961922,&
                62467957194401269043877107275048102390.895523597457,&
                23189706772547915061505504953922979530.901129967519,&
                86188088225875314529584099251203829009.407770775672,&
                11306739708304724483816533873502340845.647058077308,&
                82959174767140363198008187129011875491.310547126581,&
                97623331044818386269515456334926366572.897563400500,&
                42846280183517070527831839425882145521.227251250327,&
                55121603546981200581762165212827652751.691296897789,&
                32238195734329339946437501907836945765.883352399886,&
                75506164965184775180738168837861091527.357929701337,&
                62177842752192623401942399639168044983.993173312731,&
                32924185707147349566916674687634660915.035914677504,&
                99518671430235219628894890102423325116.913619626622,&
                73267460800591547471830798392868535206.946944540724,&
                76841822524674417161514036427982273348.055556214818,&
                97142617910342598647204516893989422179.826088076852,&
                87783646182799346313767754307809363333.018982642090,&
                10848802521674670883215120185883543223.812876952786,&
                71329612474782464538636993009049310363.619763878039,&
                62184073572399794223406235393808339651.327408011116,&
                66627891981488087797941876876144230030.984490851411,&
                60661826293682836764744779239180335110.989069790714,&
                85786944089552990653640447425576083659.976645795096,&
                66024396409905389607120198219976047599.490197230297,&
                64913982680032973156037120041377903785.566085089252,&
                16730939319872750275468906903707539413.042652315011,&
                94809377245048795150954100921645863754.710598436791,&
                78639167021187492431995700641917969777.599028300699,&
                15368713711936614952811305876380278410.754449733078,&
                40789923115535562561142322423255033685.442488917353,&       
                44889911501440648020369068063960672322.193204149535,&
                41503128880339536053299340368006977710.650566631954,&
                81234880673210146739058568557934581403.627822703280,&
                82616570773948327592232845941706525094.512325230608,&
                22918802058777319719839450180888072429.661980811197,&
                77158542502016545090413245809786882778.948721859617,&
                72107838435069186155435662884062257473.692284509516,&
                20849603980134001723930671666823555245.252804609722,&
                53503534226472524250874054075591789781.264330331690/)

                arraydec = (/&
                0.510135740250,&
                0.417018260538,&
                0.726617309629,&
                0.406154908250,&
                0.778063246676,&
                0.734361726757,&
                0.275889832738,&
                0.670272189318,&
                0.711603153276,&
                0.686757934951,&
                0.079324193331,&
                0.347944758178,&
                0.245598838407,&
                0.458628227828,&
                0.151170094390,&
                0.190104293586,&
                0.420517255829,&
                0.714970056938,&
                0.294438377604,&
                0.217779295145,&
                0.885258350721,&
                0.877223636045,&
                0.860733969412,&
                0.940512689692,&
                0.642563049483,&
                0.732956340691,&
                0.197107928209,&
                0.330393682126,&
                0.534375946515,&
                0.219257766954,&
                0.097864797581,&
                0.497806860984,&
                0.675590848030,&
                0.320693150332,&
                0.326235447210,&
                0.042633987085,&
                0.760315001271,&
                0.198746004375,&
                0.742552737307,&
                0.167401875275,&
                0.048770190271,&
                0.061632866526,&
                0.140746255962,&
                0.322810917141,&
                0.036638784583,&
                0.285025333403,&
                0.468420090470,&
                0.181441773470,&
                0.512486764533,&
                0.952310058822,&
                0.953868218774,&
                0.064507995236,&
                0.910326572967,&
                0.207058596522,&
                0.534765745501,&
                0.403798879715,&
                0.373226651381,&
                0.251122869539,&
                0.159879593635,&
                0.580388584903,&
                0.993532961922,&
                0.895523597457,&
                0.901129967519,&
                0.407770775672,&
                0.647058077308,&
                0.310547126581,&
                0.897563400500,&
                0.227251250327,&
                0.691296897789,&
                0.883352399886,&
                0.357929701337,&
                0.993173312731,&
                0.035914677504,&
                0.913619626622,&
                0.946944540724,&
                0.055556214818,&
                0.826088076852,&
                0.018982642090,&
                0.812876952786,&
                0.619763878039,&
                0.327408011116,&
                0.984490851411,&
                0.989069790714,&
                0.976645795096,&
                0.490197230297,&
                0.566085089252,&
                0.042652315011,&
                0.710598436791,&
                0.599028300699,&
                0.754449733078,&
                0.442488917353,&       
                0.193204149535,&
                0.650566631954,&
                0.627822703280,&
                0.512325230608,&
                0.661980811197,&
                0.948721859617,&
                0.692284509516,&
                0.252804609722,&
                0.264330331690/)

                do row=1,100
                        sigma = sum(array)
                        sigmadec = sum(arraydec)
                end do

                
                sigma = sigma*10e11+ sigmadec*10e13
                !once again this addition doesn't work, so add decimals
                !again.........
justadd =  1.0
justadd= justadd &
        +  37107287533902102798797998220837590246.510135740250&
        +  46376937677490009712648124896970078050.417018260538&
        +  74324986199524741059474233309513058123.726617309629&
        +  91942213363574161572522430563301811072.406154908250&
        +  23067588207539346171171980310421047513.778063246676&
        +  89261670696623633820136378418383684178.734361726757&
        +  28112879812849979408065481931592621691.275889832738&
        +  44274228917432520321923589422876796487.670272189318&
        +  47451445736001306439091167216856844588.711603153276&
        +  70386486105843025439939619828917593665.686757934951&
        +  62176457141856560629502157223196586755.079324193331&
        +  64906352462741904929101432445813822663.347944758178&
        +  92575867718337217661963751590579239728.245598838407&
        +  58203565325359399008402633568948830189.458628227828&
        +  80181199384826282014278194139940567587.151170094390&
        +  35398664372827112653829987240784473053.190104293586&
        +  86515506006295864861532075273371959191.420517255829&
        +  71693888707715466499115593487603532921.714970056938&
        +  54370070576826684624621495650076471787.294438377604&
        +  53282654108756828443191190634694037855.217779295145&
        +  36123272525000296071075082563815656710.885258350721&
        +  45876576172410976447339110607218265236.877223636045&
        +  17423706905851860660448207621209813287.860733969412&
        +  81142660418086830619328460811191061556.940512689692&
        +  51934325451728388641918047049293215058.642563049483&
        +  62467221648435076201727918039944693004.732956340691&
        +  15732444386908125794514089057706229429.197107928209&
        +  55037687525678773091862540744969844508.330393682126&
        +  18336384825330154686196124348767681297.534375946515&
        +  80386287592878490201521685554828717201.219257766954&
        +  78182833757993103614740356856449095527.097864797581&
        +  16726320100436897842553539920931837441.497806860984&
        +  48403098129077791799088218795327364475.675590848030&
        +  87086987551392711854517078544161852424.320693150332&
        +  59959406895756536782107074926966537676.326235447210&
        +  69793950679652694742597709739166693763.042633987085&
        +  41052684708299085211399427365734116182.760315001271&
        +  65378607361501080857009149939512557028.198746004375&
        +  35829035317434717326932123578154982629.742552737307&
        +  94953759765105305946966067683156574377.167401875275&
        +  88902802571733229619176668713819931811.048770190271&
        +  25267680276078003013678680992525463401.061632866526&
        +  36270218540497705585629946580636237993.140746255962&
        +  24074486908231174977792365466257246923.322810917141&
        +  91430288197103288597806669760892938638.285025333403&
        +  34413065578016127815921815005561868836.468420090470&
        +  23053081172816430487623791969842487255.036638784583&
        +  11487696932154902810424020138335124462.181441773470&
        +  63783299490636259666498587618221225225.512486764533&
        +  67720186971698544312419572409913959008.952310058822&
        +  95548255300263520781532296796249481641.953868218774&
        +  76085327132285723110424803456124867697.064507995236&
        +  37774242535411291684276865538926205024.910326572967&
        +  23701913275725675285653248258265463092.207058596522&
        +  29798860272258331913126375147341994889.534765745501&
        +  18495701454879288984856827726077713721.403798879715&
        +  38298203783031473527721580348144513491.373226651381&
        +  34829543829199918180278916522431027392.251122869539&
        +  40957953066405232632538044100059654939.159879593635&
        +  29746152185502371307642255121183693803.580388584903&
        +  41698116222072977186158236678424689157.993532961922&
        +  62467957194401269043877107275048102390.895523597457&
        +  23189706772547915061505504953922979530.901129967519&
        +  86188088225875314529584099251203829009.407770775672&
        +  11306739708304724483816533873502340845.647058077308&
        +  82959174767140363198008187129011875491.310547126581&
        +  97623331044818386269515456334926366572.897563400500&
        +  42846280183517070527831839425882145521.227251250327&
        +  55121603546981200581762165212827652751.691296897789&
        +  32238195734329339946437501907836945765.883352399886&
        +  75506164965184775180738168837861091527.357929701337&
        +  62177842752192623401942399639168044983.993173312731&
        +  32924185707147349566916674687634660915.035914677504&
        +  99518671430235219628894890102423325116.913619626622&
        +  73267460800591547471830798392868535206.946944540724&
        +  76841822524674417161514036427982273348.055556214818&
        +  97142617910342598647204516893989422179.826088076852&
        +  87783646182799346313767754307809363333.018982642090&
        +  10848802521674670883215120185883543223.812876952786&
        +  71329612474782464538636993009049310363.619763878039&
        +  62184073572399794223406235393808339651.327408011116&
        +  66627891981488087797941876876144230030.984490851411&
        +  60661826293682836764744779239180335110.989069790714&
        +  85786944089552990653640447425576083659.976645795096&
        +  66024396409905389607120198219976047599.490197230297&
        +  64913982680032973156037120041377903785.566085089252&
        +  16730939319872750275468906903707539413.042652315011&
        +  94809377245048795150954100921645863754.710598436791&
        +  78639167021187492431995700641917969777.599028300699&
        +  15368713711936614952811305876380278410.754449733078&
        +  40789923115535562561142322423255033685.442488917353&
        +  44889911501440648020369068063960672322.193204149535&
        +  41503128880339536053299340368006977710.650566631954&
        +  81234880673210146739058568557934581403.627822703280&
        +  82616570773948327592232845941706525094.512325230608&
        +  22918802058777319719839450180888072429.661980811197&
        +  77158542502016545090413245809786882778.948721859617&
        +  72107838435069186155435662884062257473.692284509516&
        +  20849603980134001723930671666823555245.252804609722&
        +  53503534226472524250874054075591789781.264330331690
                
!answer is still wrong!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!lets try reconstructing the full number before adding

do row=1,100
        fullnum = array(row)*10e11+arraydec(row)*10e11
        sumfullnum = sumfullnum + fullnum
        fullnum=0
end do
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!no dice
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                write(*,*) sigma
                write(*,*) justadd*10e11
                write(*,*) sumfullnum





        end subroutine



end program

