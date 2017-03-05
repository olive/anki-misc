# coding: utf-8

import sqlite3
import sys
import json
import re
from datetime import datetime
#1,3:32
#2,0:55?
#3,0:39,12:40,20:00,21:55
#6,16:00
heisig = [
    "一二三四五六七八九十口日月田目古吾冒朋明唱晶品呂昌早旭世胃旦",
    "胆亘凹凸旧自白百中千舌升昇丸寸肘専博占上下卓朝嘲只貝唄貞員貼",
    "見児元頁頑凡負万句肌旬勺的首乙乱直具真工左右有賄貢項刀刃切召",
    "昭則副別丁町可頂子孔了女好如母貫兄呪克小少大多夕汐外名石肖硝",
    "砕砂妬削光太器臭嗅妙省厚奇川州順水氷永泉腺原願泳沼沖汎江汰汁",
    "沙潮源活消況河泊湖測土吐圧埼垣填圭封涯寺時均火炎煩淡灯畑災灰",
    "点照魚漁里黒墨鯉量厘埋同洞胴向尚字守完宣宵安宴寄富貯木林森桂",
    "柏枠梢棚杏桐植椅枯朴村相机本札暦案燥未末昧沫味妹朱株若草苦苛",
    "寛薄葉模漠墓暮膜苗兆桃眺犬状黙然荻狩猫牛特告先洗介界茶脊合塔",
    "王玉宝珠現玩狂旺皇呈全栓理主注柱金銑鉢銅釣針銘鎮道導辻迅造迫",
    "逃辺巡車連軌輸喩前煎各格賂略客額夏処条落冗冥軍輝運冠夢坑高享",
    "塾熟亭京涼景鯨舎周週士吉壮荘売学覚栄書津牧攻敗枚故敬言警計詮",
    "獄訂訃討訓詔詰話詠詩語読調談諾諭式試弐域賊栽載茂戚成城誠威滅",
    "減蔑桟銭浅止歩渉頻肯企歴武賦正証政定錠走超赴越是題堤建鍵延誕",
    "礎婿衣裁装裏壊哀遠猿初巾布帆幅帽幕幌錦市柿姉肺帯滞刺制製転芸",
    "雨雲曇雷霜冬天妖沃橋嬌立泣章競帝諦童瞳鐘商嫡適滴敵匕叱匂頃北",
    "背比昆皆楷諧混渇謁褐喝葛旨脂詣壱毎敏梅海乞乾腹複欠吹炊歌軟次",
    "茨資姿諮賠培剖音暗韻識鏡境亡盲妄荒望方妨坊芳肪訪放激脱説鋭曽",
    "増贈東棟凍妊廷染燃賓歳県栃地池虫蛍蛇虹蝶独蚕風己起妃改記包胞",
    "砲泡亀電竜滝豚逐遂家嫁豪腸場湯羊美洋詳鮮達羨差着唯堆椎誰焦礁",
    "集准進雑雌準奮奪確午許歓権観羽習翌曜濯曰困固錮国団因姻咽園回",
    "壇店庫庭庁床麻磨心忘恣忍認忌志誌芯忠串患思恩応意臆想息憩恵恐",
    "惑感憂寡忙悦恒悼悟怖慌悔憎慣愉惰慎憾憶惧憧憬慕添必泌手看摩我",
    "義議犠抹拭拉抱搭抄抗批招拓拍打拘捨拐摘挑指持拶括揮推揚提損拾",
    "担拠描操接掲掛捗研戒弄械鼻刑型才財材存在乃携及吸扱丈史吏更硬",
    "梗又双桑隻護獲奴怒友抜投没股設撃殻支技枝肢茎怪軽叔督寂淑反坂",
    "板返販爪妥乳浮淫将奨采採菜受授愛曖払広勾拡鉱弁雄台怠治冶始胎",
    "窓去法会至室到致互棄育撤充銃硫流允唆出山拙岩炭岐峠崩密蜜嵐崎",
    "崖入込分貧頒公松翁訟谷浴容溶欲裕鉛沿賞党堂常裳掌皮波婆披破被",
    "残殉殊殖列裂烈死葬瞬耳取趣最撮恥職聖敢聴懐慢漫買置罰寧濁環還",
    "夫扶渓規替賛潜失鉄迭臣姫蔵臓賢腎堅臨覧巨拒力男労募劣功勧努勃",
    "励加賀架脇脅協行律復得従徒待往征径彼役徳徹徴懲微街桁衡稿稼程",
    "税稚和移秒秋愁私秩秘称利梨穫穂稲香季委秀透誘稽穀菌萎米粉粘粒",
    "粧迷粋謎糧菊奥数楼類漆膝様求球救竹笑笠笹箋筋箱筆筒等算答策簿",
    "築篭人佐侶但住位仲体悠件仕他伏伝仏休仮伎伯俗信佳依例個健側侍",
    "停値倣傲倒偵僧億儀償仙催仁侮使便倍優伐宿傷保褒傑付符府任賃代",
    "袋貸化花貨傾何荷俊傍俺久畝囚内丙柄肉腐座挫卒傘匁以似併瓦瓶宮",
    "営善膳年夜液塚幣蔽弊喚換融施旋遊旅勿物易賜尿尼尻泥塀履屋握屈",
    "掘堀居据裾層局遅漏刷尺尽沢訳択昼戸肩房扇炉戻涙雇顧啓示礼祥祝",
    "福祉社視奈尉慰款禁襟宗崇祭察擦由抽油袖宙届笛軸甲押岬挿申伸神",
    "捜果菓課裸斤析所祈近折哲逝誓斬暫漸断質斥訴昨詐作雪録剥尋急穏",
    "侵浸寝婦掃当彙争浄事唐糖康逮伊君群耐需儒端両満画歯曲曹遭漕槽",
    "斗料科図用庸備昔錯借惜措散廿庶遮席度渡奔噴墳憤焼暁半伴畔判拳",
    "券巻圏勝藤謄片版之乏芝不否杯矢矯族知智挨矛柔務霧班帰弓引弔弘",
    "強弥弱溺沸費第弟巧号朽誇顎汚与写身射謝老考孝教拷者煮著箸署暑",
    "諸猪渚賭峡狭挟頬追阜師帥官棺管父釜交効較校足促捉距路露跳躍践",
    "踏踪骨滑髄禍渦鍋過阪阿際障隙随陪陽陳防附院陣隊墜降階陛隣隔隠",
    "堕陥穴空控突究窒窃窟窪搾窯窮探深丘岳兵浜糸織繕縮繁縦緻線綻締",
    "維羅練緒続絵統絞給絡結終級紀紅納紡紛紹経紳約細累索総綿絹繰継",
    "緑縁網緊紫縛縄幼後幽幾機畿玄畜蓄弦擁滋慈磁系係孫懸遜却脚卸御",
    "服命令零齢冷領鈴勇湧通踊疑擬凝範犯氾厄危宛腕苑怨柳卵留瑠貿印",
    "臼毀興酉酒酌酎酵酷酬酪酢酔配酸猶尊豆頭短豊鼓喜樹皿血盆盟盗温",
    "蓋監濫鑑藍猛盛塩銀恨根即爵節退限眼良朗浪娘食飯飲飢餓飾餌館餅",
    "養飽既概慨平呼坪評刈刹希凶胸離璃殺爽純頓鈍辛辞梓宰壁璧避新薪",
    "親幸執摯報叫糾収卑碑陸睦勢熱菱陵亥核刻該骸劾述術寒塞醸譲壌嬢",
    "毒素麦青精請情晴清静責績積債漬表俵潔契喫害轄割憲生星醒姓性牲",
    "産隆峰蜂縫拝寿鋳籍春椿泰奏実奉俸棒謹僅勤漢嘆難華垂唾睡錘乗剰",
    "今含貪吟念捻琴陰予序預野兼嫌鎌謙廉西価要腰票漂標栗慄遷覆煙南",
    "楠献門問閲閥間闇簡開閉閣閑聞潤欄闘倉創非俳排悲罪輩扉侯喉候決",
    "快偉違緯衛韓干肝刊汗軒岸幹芋宇余除徐叙途斜塗束頼瀬勅疎辣速整",
    "剣険検倹重動腫勲働種衝薫病痴痘症瘍痩疾嫉痢痕疲疫痛癖匿匠医匹",
    "区枢殴欧抑仰迎登澄発廃僚瞭寮療彫形影杉彩彰彦顔須膨参惨修珍診",
    "文対紋蚊斑斉剤済斎粛塁楽薬率渋摂央英映赤赦変跡蛮恋湾黄横把色",
    "絶艶肥甘紺某謀媒欺棋旗期碁基甚勘堪貴遺遣潰舞無組粗租狙祖阻査",
    "助宜畳並普譜湿顕繊霊業撲僕共供異翼戴洪港暴爆恭選殿井丼囲耕亜",
    "悪円角触解再講購構溝論倫輪偏遍編冊柵典氏紙婚低抵底民眠捕哺浦",
    "蒲舗補邸郭郡郊部都郵邦那郷響郎廊盾循派脈衆逓段鍛后幻司伺詞飼",
    "嗣舟舶航舷般盤搬船艦艇瓜弧孤繭益暇敷来気汽飛沈枕妻凄衰衷面麺",
    "革靴覇声眉呉娯誤蒸承函極牙芽邪雅釈番審翻藩毛耗尾宅託為偽畏長",
    "張帳脹髪展喪巣単戦禅弾桜獣脳悩厳鎖挙誉猟鳥鳴鶴烏蔦鳩鶏島暖媛",
    "援緩属嘱偶遇愚隅逆塑遡岡鋼綱剛缶陶揺謡鬱就蹴懇墾貌免逸晩勉象",
    "像馬駒験騎駐駆駅騒駄驚篤罵騰虎虜膚虚戯虞慮劇虐鹿麓薦慶麗熊能",
    "態寅演辰辱震振娠唇農濃送関咲鬼醜魂魔魅塊襲嚇朕雰箇錬遵罷屯且",
    "藻隷癒璽潟丹丑羞卯巳此柴些砦髭禽檎憐燐麟鱗奄庵掩悛駿峻竣犀皐",
    "畷綴鎧凱呑韮籤懺芻雛趨尤厖或兎也巴疋菫曼云莫而倭侠倦俄佃仔仇",
    "伽儲僑倶侃偲侭脩倅做冴凋凌凛凧凪夙鳳剽劉剃厭雁贋厨仄哨咎囁喋",
    "嘩噂咳喧叩嘘啄吠吊噛叶吻吃噺噌邑呆喰埴坤壕垢坦埠堰堵嬰姦婢婉",
    "娼妓娃姪嬬姥姑姐嬉孕孜宥寓宏牢宋宍屠屁屑屡屍屏嵩崚嶺嵌帖幡幟",
    "庖廓庇鷹庄廟彊弛粥挽撞扮捏掴捺掻撰揃捌按播揖托捧撚挺擾撫撒擢",
    "摺捷抉怯惟惚怜惇恰恢悌澪洸滉漱洲洵滲洒沐泪渾涜梁澱洛汝漉瀕濠",
    "溌湊淋浩汀鴻潅溢湛淳渥灘汲瀞溜渕沌濾濡淀涅斧爺猾猥狡狸狼狽狗",
    "狐狛獅狒莨茉莉苺萩藝薙蓑苔蕩蔓蓮芙蓉蘭芦薯菖蕉蕎蕗茄蔭蓬芥萌",
    "葡萄蘇蕃苓菰蒙茅芭苅葱葵葺蕊茸蒔芹苫蒼藁蕪藷薮蒜蕨蔚茜莞蒐菅",
    "葦迪辿這迂遁逢遥遼逼迄逗鄭隕隈憑惹悉忽惣愈恕昴晋晟暈暉旱晏晨",
    "晒晃曝曙昂昏晦膿腑胱胚肛脆肋腔肱胡楓楊椋榛櫛槌樵梯柑杭柊柚椀",
    "栂柾榊樫槙楢橘桧棲栖桔杜杷梶杵杖樽櫓橿杓李棉楯榎樺槍柘梱枇樋",
    "橇槃栞椰檀樗槻椙彬桶楕樒毬燿燎炬焚灸煽煤煉燦灼烙焔烹牽牝牡琳",
    "琉瑳琢珊瑚瑞玖瑛玲畢畦痒痰疹痔癌痺眸眩雉矩磐碇碧硯砥碗碍碩磯",
    "砺碓禦祷祐祇祢禄禎秤黍禿稔稗穣稜稀穆窺窄穿竃竪颯站靖妾衿袷袴",
    "襖笙筏簾箪竿箆箔笥箭筑篠纂竺箕笈篇筈簸粕糟糊籾糠糞粟繋綸絨絆",
    "緋綜紐紘纏絢繍紬綺綾絃縞綬紗舵聯聡聘耽耶蚤蟹蛋蟄蝿蟻蝋蝦蛸螺",
    "蝉蛙蛾蛤蛭蛎罫袈裟截哉詢諄讐諌諒讃訊訣詫誼謬訝諺誹謂諜註譬轟",
    "輔輻輯豹賎貰賑贖躓蹄蹟跨跪醤醍醐醇麹釦銚鋤鋸錐鍬鋲錫錨釘鑓鋒",
    "鎚鉦錆鍾鋏閃悶閤雫霞翰斡鞍鞭鞘鞄靭鞠顛穎頗頌頚餐饗蝕飴駕騨馳",
    "騙馴駁駈驢鰻鯛鰯鱒鮭鮪鮎鯵鱈鯖鮫鰹鰍鰐鮒鮨鰭鴎鵬鸚鵡鵜鷺鷲鴨",
    "鳶梟塵麒舅鼠鑿艘瞑暝坐朔曳洩彗慧爾嘉兇兜靄劫歎輿歪翠黛鼎鹵鹸",
    "虔燕嘗殆牌覗齟齬秦雀隼耀夷嚢暢廻欣毅斯匙匡肇麿叢肴斐卿翫於套",
    "叛尖壷叡酋鴬赫臥甥瓢琵琶叉乖畠圃丞亮胤疏膏魁馨牒瞥睾巫敦奎翔",
    "皓黎赳已棘祟甦剪躾夥鼾陀粁糎粍噸哩浬吋呎梵薩菩唖牟迦珈琲檜轡",
    "淵伍什萬邁燭逞燈裡薗鋪嶋峯埜龍寵聾慾嶽國脛勁祀祓躇壽躊饅嘔鼈",

]

freq2501 = [
    "日一国会人年大十二本中長出三同時政事自行社見月分議後前民生連",
    "五発間対上部東者党地合市業内相方四定今回新場金員九入選立開手",
    "米力学問高代明実円関決子動京全目表戦経通外最言氏現理調体化田",
    "当八六約主題下首意法不来作性的要用制治度務強気小七成期公持野",
    "協取都和統以機平総加山思家話世受区領多県続進正安設保改数記院",
    "女初北午指権心界支第産結百派点教報済書府活原先共得解名交資予",
    "川向際査勝面委告軍文反元重近千考判認画海参売利組知案道信策集",
    "在件団別物側任引使求所次水半品昨論計死官増係感特情投示変打男",
    "基私各始島直両朝革価式確村提運終挙果西勢減台広容必応演電歳住",
    "争談能無再位置企真流格有疑口過局少放税検藤町常校料沢裁状工建",
    "語球営空職証土与急止送援供可役構木割聞身費付施切由説転食比難",
    "防補車優夫研収断井何南石足違消境神番規術護展態導鮮備宅害配副",
    "算視条幹独警宮究育席輸訪楽起万着乗店述残想線率病農州武声質念",
    "待試族象銀域助労例衛然早張映限親額監環験追審商葉義伝働形景落",
    "欧担好退準賞訴辺造英被株頭技低毎医復仕去姿味負閣韓渡失移差衆",
    "個門写評課末守若脳極種美岡影命含福蔵量望松非撃佐核観察整段横",
    "融型白深字答夜製票況音申様財港識注呼渉達良響阪帰針専推谷古候",
    "史天階程満敗管値歌買突兵接請器士光討路悪科攻崎督授催細効図週",
    "積丸他及湾録処省旧室憲太橋歩離岸客風紙激否周師摘材登系批郎母",
    "易健黒火戸速存花春飛殺央券赤号単盟座青破編捜竹除完降超責並療",
    "従右修捕隊危採織森競拡故館振給屋介読弁根色友苦就迎走販園具左",
    "異歴辞将秋因献厳馬愛幅休維富浜父遺彼般未塁貿講邦舞林装諸夏素",
    "亡劇河遣航抗冷模雄適婦鉄寄益込顔緊類児余禁印逆王返標換久短油",
    "妻暴輪占宣背昭廃植熱宿薬伊江清習険頼僚覚吉盛船倍均億途圧芸許",
    "皇臨踏駅署抜壊債便伸留罪停興爆陸玉源儀波創障継筋狙帯延羽努固",
    "闘精則葬乱避普散司康測豊洋静善逮婚厚喜齢囲卒迫略承浮惑崩順紀",
    "聴脱旅絶級幸岩練押軽倒了庁博城患締等救執層版老令角絡損房募曲",
    "撤裏払削密庭徒措仏績築貨志混載昇池陣我勤為血遅抑幕居染温雑招",
    "奈季困星傷永択秀著徴誌庫弾償刊像功拠香欠更秘拒刑坂刻底賛塚致",
    "抱繰服犯尾描布恐寺鈴盤息宇項喪伴遠養懸戻街巨震願絵希越契掲躍",
    "棄欲痛触邸依籍汚縮還枚属笑互複慮郵束仲栄札枠似夕恵板列露沖探",
    "逃借緩節需骨射傾届曜遊迷夢巻購揮君燃充雨閉緒跡包駐貢鹿弱却端",
    "賃折紹獲郡併草徹飲貴埼衝焦奪雇災浦暮替析預焼簡譲称肉納樹挑章",
    "臓律誘紛貸至宗促慎控贈智握照宙酒俊銭薄堂渋群銃悲秒操携奥診詰",
    "託晴撮誕侵括掛謝双孝刺到駆寝透津壁稲仮暗裂敏鳥純是飯排裕堅訳",
    "盗芝綱吸典賀扱顧弘看訟戒祉誉歓勉奏勧騒翌陽閥甲快縄片郷敬揺免",
    "既薦隣悩華泉御範隠冬徳皮哲漁杉里釈己荒貯硬妥威豪熊歯滞微隆埋",
    "症暫忠倉昼茶彦肝柱喚沿妙唱祭袋阿索誠忘襲雪筆吹訓懇浴俳童宝柄",
    "驚麻封胸娘砂李塩浩誤剤瀬趣陥斎貫仙慰賢序弟旬腕兼聖旨即洗柳舎",
    "偽較覇兆床畑慣詳毛緑尊抵脅祝礼窓柔茂犠旗距雅飾網竜詩昔繁殿濃",
    "翼牛茨潟敵魅嫌魚斉液貧敷擁衣肩圏零酸兄罰怒滅泳礎腐祖幼脚菱荷",
    "潮梅泊尽杯僕桜滑孤黄煕炎賠句寿鋼頑甘臣鎖彩摩浅励掃雲掘縦輝蓄",
    "軸巡疲稼瞬捨皆砲軟噴沈誇祥牲秩帝宏唆鳴阻泰賄撲凍堀腹菊絞乳煙",
    "縁唯膨矢耐恋塾漏紅慶猛芳懲郊剣腰炭踊幌彰棋丁冊恒眠揚冒之勇曽",
    "械倫陳憶怖犬菜耳潜珍梨仁克岳概拘墓黙須偏雰卵遇湖諮狭喫卓干頂",
    "虫刷亀糧梶湯箱簿炉牧殊殖艦溶輩穴奇慢鶴謀暖昌拍朗丈鉱寛覆胞泣",
    "涙隔浄匹没暇肺孫貞靖鑑飼陰銘鋭随烈尋渕稿枝丹啓也丘棟壌漫玄粘",
    "悟舗妊塗熟軒旭恩毒騰往豆遂晩狂叫栃岐陛緯培衰艇屈径淡抽披廷錦",
    "准暑拝磯奨妹浸剰胆氷繊駒乾虚棒寒孜霊帳悔諭祈惨虐翻墜沼据肥徐",
    "糖搭姉髪忙盾脈滝拾軌俵妨盧粉擦鯨漢糸荘諾雷漂懐勘綿栽才拐笠駄",
    "添汗冠斜銅鏡聡浪亜覧詐壇勲魔酬紫湿曙紋卸奮趙欄逸涯拓眼瓶獄筑",
    "尚阜彫咲穏顕巧矛垣召欺釣缶萩粧隻葛脂粛栗愚蒸嘉遭架篠鬼庶肌稚",
    "靴菅滋幻煮姫誓耕把践呈疎仰鈍恥剛疾征砕謡嫁謙后嘆俣菌鎌巣泥頻",
    "琴班淵棚潔酷宰廊寂辰隅偶霞伏灯柏辛磨碁俗漠邪晶辻麦墨鎮洞履劣",
    "那殴娠奉憂朴亭姓淳荻筒鼻嶋怪粒詞鳩柴偉酔惜穫佳潤悼乏胃該赴桑",
    "桂髄虎盆晋穂壮堤飢傍疫累痴搬畳晃癒桐寸郭机尿凶吐宴鷹賓虜膚陶",
    "鐘憾畿猪紘磁弥昆粗訂芽尻庄傘敦騎寧濯循忍磐猫怠如寮祐鵬塔沸鉛",
    "珠凝苗獣哀跳灰匠菓垂蛇澄縫僧幾眺唐亘呉凡憩鄭芦龍媛溝恭刈睡錯",
    "伯帽笹穀柿陵霧魂枯弊釧妃舶餓腎窮掌麗綾臭釜悦刃縛暦宜盲粋辱毅",
    "轄猿弦嶌稔窒炊洪摂飽函冗涼桃狩舟貝朱渦紳枢碑鍛刀鼓裸鴨符猶塊",
    "旋弓幣膜扇脇腸憎槽鍋慈皿肯樋楊伐駿漬燥糾亮墳坪畜紺慌娯吾椿舌",
    "羅坊峡俸厘峰圭醸蓮弔乙倶汁尼遍堺衡呆薫瓦猟羊窪款閲雀偵喝敢畠",
    "胎酵憤豚遮扉硫赦挫挟窃泡瑞又慨紡恨肪扶戯伍忌濁奔斗蘭蒲迅肖鉢",
    "朽殻享秦茅藩沙輔曇媒鶏禅嘱胴粕冨迭挿湘嵐椎灘堰獅姜絹陪剖譜郁",
    "悠淑帆暁鷲傑楠笛芥其玲奴誰錠拳翔遷拙侍尺峠篤肇渇榎俺劉幡諏叔",
    "雌亨堪叙酢吟逓痕嶺袖甚喬崔妖琵琶聯蘇闇崇漆岬癖愉寅捉礁乃洲屯",
    "樽樺槙薩姻巌淀麹賭擬塀唇睦閑胡幽峻曹哨詠炒屏卑侮鋳抹尉槻隷禍",
    "蝶酪茎汎頃帥梁逝滴汽謎琢箕匿爪芭逗苫鍵襟蛍楢蕉兜寡琉痢庸朋坑",
    "姑烏藍僑賊搾奄臼畔遼唄孔橘漱呂桧拷宋嬢苑巽杜渓翁藝廉牙謹瞳湧",
    "欣窯褒醜魏篇升此峯殉煩巴禎枕劾菩堕丼租檜稜牟桟榊錫荏惧倭婿慕",
    "廟銚斐罷矯某囚魁薮虹鴻泌於赳漸逢凧鵜庵膳蚊葵厄藻萬禄孟鴈狼嫡",
    "呪斬尖翫嶽尭怨卿串已嚇巳凸暢腫粟燕韻綴埴霜餅魯硝牡箸勅芹杏迦",
    "棺儒鳳馨斑蔭焉慧祇摯愁鷺楼彬袴匡眉苅讃尹欽薪湛堆狐褐鴎瀋挺賜",
    "嵯雁佃綜繕狛壷橿栓翠鮎芯蜜播榛凹艶帖伺桶惣股匂鞍蔦玩萱梯雫絆",
    "錬湊蜂隼舵渚珂煥衷逐斥稀癌峨嘘旛篭芙詔皐雛娼篆鮫椅惟牌宕喧佑",
    "蒋樟耀黛叱櫛渥挨憧濡槍宵襄妄惇蛋脩笘宍甫酌蚕壕嬉囃蒼餌簗峙粥",
    "舘銕鄒蜷暉捧頒只肢箏檀鵠凱彗謄諌樫噂脊牝梓洛醍砦丑笏蕨噺抒嗣",
    "隈叶凄汐絢叩嫉朔蔡膝鍾仇伽夷恣瞑畝抄杭寓麺戴爽裾黎惰坐鍼蛮塙",
    "冴旺葦礒咸萌饗歪冥偲壱瑠韮漕杵薔膠允眞蒙蕃呑侯碓茗麓瀕蒔鯉竪",
    "弧稽瘤澤溥遥蹴或訃矩厦冤剥舜侠贅杖蓋畏喉汪猷瑛搜曼附彪撚噛卯",
    "桝撫喋但溢闊藏浙彭淘剃揃綺徘巷竿蟹芋袁舩拭茜凌頬厨犀簑皓甦洸",
    "毬檄姚蛭婆叢椙轟贋洒貰儲緋貼諜鯛蓼甕喘怜溜邑鉾倣碧燈諦煎瓜緻",
    "哺槌啄穣嗜偕罵酉蹄頚胚牢糞悌吊楕鮭乞倹嗅詫鱒蔑轍醤惚廣藁柚舛",
    "縞謳杞鱗繭釘弛狸壬硯蝦"
]

numconv = {
#"1":"１",
#"2":"２",
#"3":"３",
#"4":"４",
#"5":"５",
#"6":"６",
#"7":"７",
#"8":"８",
#"9":"９",
#"0":"０",
#".":"分",
#"%":"％"
}

def conv(i):
    i = str(i)
    if i in numconv:
        return numconv[i]
    else:
        return i

heisig = "".join(heisig)
freq2501 = "".join(freq2501)

debug = False

def extract_kanji(s):
    # \u3000-\u303f punctuation
    # \u3040-\u309f hiragana
    # \u30a0-\u30ff katakana
    # \uff00-\uff9f fullwidth
    reg = "[\u4e00-\u9faf\u3400-\u4dbf]"

    matches = "".join([ match for match in re.findall(reg,s) ])

    res = []
    seen = dict()
    for char in matches:
        if char not in seen:
            res.append(char)
        seen[char] = 1
    return res

def get_decks_s(db, matches=None):
    d = dict()
    if not matches:
        matches = [None]
    for match in matches:
        decks = get_decks(db, match)
        d.update(decks)
    return d

def get_all_deckdata(db):
    deckdata = None
    for row in db.execute("select decks from col"):
        deckdata = row[0]
        break
    if not deckdata:
        print("Failed to import decks from sqlite")
        exit(1)

    return deckdata

def get_decks(db, match=None):
    deckdata = get_all_deckdata(db)

    data = json.loads(deckdata)
    def func(v):
        if match:
            return match in v
        else:
            return True
    return { k:v for k,v in [ (k,data[k]['name']) for k in data ] if func(v) }

def diff(known, body):
    filtered = [ k for k in known if k in heisig ]
    nonheisig = [ k for k in known if k not in heisig ]

    print("%4d Known heisig kanji (%05.2f%%)" % (len(filtered), 100*len(filtered)/len(heisig)))
    print("%4d Known non-heisig kanji" % len(nonheisig))

    needed = [ k for k in body if k not in known ]
    return needed

def show_query(db, s):
    for row in db.execute(s):
        print(row)

def pprint(s):
    import pprint
    pp = pprint.PrettyPrinter(indent=4)
    pp.pprint(s)

#fix timezone
def accurate_percent(db, match, date, decktree):
    decks = get_decks(db, match)
    names = ",".join(decks)

    #for row in db.execute('SELECT * FROM sqlite_master WHERE type=\'table\''):
    #    a, b, f, d, e = row
    #    print(e)

    #for row in db.execute("WITH date as (select '{date}' as Date) SELECT revlog.ease FROM revlog cross join date where revlog.ease = 1 and julianday(strftime('%Y-%m-%d', revlog.id/1000.0, 'unixepoch')) - julianday(date.Date) = 0 limit 100".format(**locals())):
    #    print(row)
    #for row in db.execute('SELECT * FROM sqlite_master WHERE type=\'table\''):
    #    print(row)

    """ where ease == 1
    type == 0: colLearn
    type == 1: colMature
    type == 2: colRelearn
    type == 3: colCram
    """
    def collect_revs(db, date, decks):
        import os, time, datetime
        tz = "-%02d:00" % int(time.timezone/3600)
        pattern = '%Y-%m-%d'
        os.environ['TZ']='UTC'
        epoch = int(time.mktime(time.strptime(date, pattern)))
        f = """
            select
                revlog.id,
                strftime('%Y-%m-%d %H',
                         revlog.id/1000.0,
                         'unixepoch',
                         '-{tz}') as reviewDate,
                revlog.cid,
                revlog.ease as buttonNum,
                revlog.ivl as interval,
                revlog.lastIvl as lastInterval,
                revlog.time as reviewDuration,
                revlog.type,
                cards.did
            from revlog
            join cards
                on cards.id = revlog.cid
            where abs({epoch} - (revlog.id/1000.0)) < 172800
            order by cid, revlog.id desc
            """.format(**locals())
        x = 0
        l = []
        cards = {}
        for line in db.execute(f):
            x += 1
            idd = line[2]
            if idd not in cards:
                cards[idd] = []
            cards[idd].append(line)
        #has Review same day, and before a Relearn

        from collections import defaultdict
        total = defaultdict(int)
        lapses = defaultdict(int)
        maps = {}
        learnmaps = defaultdict(int)
        #any time interval decreases
        ignoreReviews = True
        for c in cards:
            #print(str(c) + " " + str(len(cards[c])))
            revlog = cards[c]
            lv = 0
            lapse = 0
            found = 0
            learned = 0
            ct = 0
            for r in reversed(revlog):
                ts,datestring,cid,bnum,ivl,livl,dur,typ,did = r
                did = str(did)
                if datestring != date:
                    pass
                else:
                    if typ == 0 and ct == 0:#this means all entries are today
                        learned = 1
                    if bnum == 1 and (not (ignoreReviews and typ in [2,0])) :
                        lapse = 1
                    found = 1
                ct += 1
            if found:
                total[did] += 1
            if lapse:
                lapses[did] += 1
            if learned:
                learnmaps[did] += 1

        for did in decks:
            #print(repr(did))
            bottom = 0
            top = 0
            bottom = total[did]
            top = bottom - lapses[did]
            #print(top,bottom)
            maps[decks[did]] = (top,bottom)
        return (maps,learnmaps)




    return collect_revs(db, date, decks)
def getp(t,b):
    if b == 0:
        percent = "N / A%" #不適用
    else:
        percent = "%04.1f%%" % (100*t/b)
    t = ""
    for p in percent:
        t += conv(p)
    return t
def print_percent(decktree, lapses):
    def get_stats(tree,name,maps):
        res = []
        def go(tree, accum, top, bottom, level):
            for k in reversed(sorted(tree.keys())):
                curdeck = k
                if level > 0:
                    curdeck = accum + "::" + k
                (p,a) = go(tree[k], curdeck, 0, 0, level+1)
                top += p
                bottom += a
            if accum in maps:
                (p,a) = maps[accum]
            else:
                (p,a) = (0,0)
            top = p+top
            bottom = a+bottom
            percent = getp(top,bottom)
            res.append((level,percent,top,bottom,accum))
            return (top,bottom)
        go(tree,name,0, 0, 0)
        return res
    res = get_stats(decktree, "三千世界",lapses)
    ignoreEmpty = True
    if res:
        print("Percent correct:")
    else:
        print("No cards studied yet today")
    maxlev = max(list(map(lambda x:x[0], res)) + [0])
    maxpc = max(list(map(lambda x:len(x[1]), res)) + [0])
    maxtop = max(list(map(lambda x:len(str(x[2])), res)) + [0])
    maxbot = max(list(map(lambda x:len(str(x[3])), res)) + [0])
    def mkfmt(s,d):
        return ("%"+str(d)+"s") % s
    for r in reversed(res):
        (lev,pc,top,bot,name) = r
        if bot == 0 and ignoreEmpty:
            continue
        part1 = ((lev)*"  ") + ((7 - len(pc))*" ") + pc
        rightbuff = ((maxlev-lev)*2)*" "
        maxsp = " "*(maxtop+maxbot+3)
        part2 = "(" + mkfmt(top,maxtop) + "/" + mkfmt(bot,maxbot) + ")"
        part3 = (lev*"- ") + name.split("::")[-1]
        print(" " + part1 + rightbuff + " " + part2 + " " + part3)

def print_learned(learned, decks):
    rev = { v: k for k, v in decks.items() }
    keys = sorted(rev.keys())
    total = sum(list(map(lambda x:learned[x], learned)))
    if keys:
        print("Learned cards:")
    for deckname in sorted(keys):
        deck = rev[deckname]
        cards = learned[rev[deckname]]
        if cards:
            p = deckname
            if len(p) > 20:
                d = deckname.split("::")
                if len(d) == 1:
                    p = d[0]
                elif len(d) == 2:
                    p = deckname
                else:
                    p = d[0] + "::…::" + d[-1]
            print("    %3d: %s" % (cards,p))
    print("       -")
    print("    %3d: Total" % total)

def get_kws(cards, decks, cardtypes, kjfield, kwfield, pdecks, ptypes, ks):
    #print(repr(cardtypes))
    res = []
    resn = []
    found = {}
    for c in cards:
        #print(repr(c['type']))
        #print(cardtypes[c['type']])
        if cardtypes[c['type']]['name'] in ptypes and decks[c['deck']] in pdecks:
            fields = c['fields']
            if kjfield in fields and kwfield in fields:
                if fields[kjfield] in ks:
                    res.append((fields[kjfield],fields[kwfield]))
                    found[fields[kjfield]] = 1
    #parse html and look for italics, # in link, etc
    for k in ks:
        if k not in found:
            res.append((k, None))
    return res

#some cards are type Audio but dont have audio...
def main():
    dbfile = sys.argv[1]
    getkw = None
    fields = None
    clozesearch = None
    difff = None
    for arg in sys.argv:
        if "-s" in arg:
            clozesearch = arg.split('=')[1]
            fields = ['Answer','Front']
        if "-k" in arg:
            getkw = arg.split('=')[1].split(',')
    if "-d" in sys.argv:
        global debug
        debug = True
    if "-diff" in sys.argv:
        difff = True
    db = sqlite3.connect('file:%s?mode=ro'%dbfile,uri=True)
    kanji = [('Lazy kanji->keyword','Kanji'),('RTK keyword->kanji','Kanji')]
    wanted = [
        ('ImageContext', 'Front'),
        ('Audio', 'Answer'),
        ('AudioContext', 'Answer'),
        ('Cloze', 'Front'),
        ('Basic', 'Front')
    ]
    decks = get_decks(db)
    (cards,cardtypes) = load_cards(db)


    print(len(cards))
    if difff:
        source = '只管打坐'
        kanji_lst = extract_by_type(cards, decks, cardtypes, kanji)
        k = extract_kanji("".join(kanji_lst))
        #learn_list =
        t = extract_by_type(cards, decks, cardtypes,wanted,source)
        tk = extract_kanji("".join(t))
        diff(k,tk)
        needed = []
        for c in tk:
            if c not in k:
                needed.append(c)
        if needed:
            print("Needed kanji from 「%s」(%d):" % (source, len(needed)))
            print("「" + "".join(needed) + "」")
        else:
            print("No kanji needed from selected decks")

    if clozesearch:
        print("Search clozes for 「%s」" % clozesearch)
        nids = {}
        found = []
        for card in cards:
            for f in card['fields']:
                if not fields or f not in fields:
                    continue
                if card['noteid'] in nids:
                    continue
                else:
                    nids[card['noteid']] = 1
                matches = re.findall("{{[^}]*"+clozesearch+"[^}]*}}",card['fields'][f])
                if matches:
                    for match in matches:
                        match = match[2:][:-2]
                        v,l,text = match.split('::')
                        cloze = v[1:]
                        found.append((card['noteid'], cloze, text))
        for i in found:
            nid,cloze,text = i
            print("nid:" + str(nid) + " card:" + cloze)
            print ("    %s" %(text))
        if not found:
            print("Not found")
    if getkw:
        kjfield = 'Kanji'
        kwfield = 'Keyword'
        pdecks = ['すべて::字::覚える::漢字','すべて::字::のんびりと覚える::漢字','すべて::字::のんびりと覚える::成分', 'すべて::字::覚える::成分']
        ptypes = [
            'Lazy kanji->keyword','RTK keyword->kanji'
        ]

        kws = get_kws(cards, decks, cardtypes, kjfield, kwfield, pdecks, ptypes, getkw)

        for k in kws:
            if k[1] is None:
                print("Not implemented ("+k[0]+")")
            else:
                print("%s: %s" % k)
    else:
        source = 'すべて'
        daysback = 1
        from datetime import datetime, timedelta
        now = datetime.now()
        decktree = sort_decks(decks.values())
        stuff = []
        for i in range(daysback):
            date = (now - timedelta(days=i)).strftime('%Y-%m-%d')#'2016-12-28'
            (lapses,learned) = accurate_percent(db, source, date, decktree)
            print_percent(decktree, lapses)
            print_learned(learned, decks)
    #print("".join(tk))
    #print(get_all_deckdata(db))

def sort_decks(decks, d=0): #returns map
    m = {}
    tt = {}
    for v in decks:
        parts = v.split("::", 1)
        if parts[0] not in m:
            m[parts[0]] = []
        if len(parts) == 2:
            m[parts[0]].append(parts[1])
        else:
            tt[parts[0]] = {}
    for k in m:
        tt[k] = sort_decks(m[k], d+1)
    return tt



def extract_by_type(allcards, decks, cardtypes, types, deck=None):
    stuff = []
    for (typ,field) in types:
        for card in allcards:
            if card['state'] != -1 and cardtypes[card['type']]['name'] == typ and (not deck or deck in decks[card['deck']]) and card['studytype'] != 0:
                stuff.append(card['fields'][field])
    return stuff


def load_cards(db):

    line = json.loads([ line for line in db.execute("select models from col") ][0][0])
    #print(json.dumps(line, sort_keys=True, indent=4, separators=(',', ': ')))
    cardtypes = dict()
    for k in line:
        e = line[k]
        idd = str(e['id'])
        cardtypes[idd] = {}
        ct = cardtypes[idd]
        ct['name'] = e['name']
        fields = []
        for i in range(50):
            try:
                x = e["flds"][i]
                fields.append((x['name'], x['ord']))
            except:
                pass
        fields.sort(key=lambda x: x[1])
        ct['fields'] = list(map(lambda x: x[0], fields))
    audio = ""
    allcards = []
    seen = {}
    query="""
          select
              n.id,
              n.flds,
              c.id,
              n.mid,
              c.queue,
              c.type,
              c.did
          from notes as n
          join cards as c
              on c.nid = n.id
    """
    for line in list(db.execute(query)):
        card = {}
        (noteid,raw,cardid,ct,queue, studytype,did) = line
        ct = str(ct)
        did = str(did)
        data = raw
        spl = data.split("\x1f")
        card['type'] = ct
        card['studytype'] = studytype
        card['state'] = queue
        card['deck'] = did
        card['noteid'] = noteid
        fields = {}
        for i in range(len(spl)):
            typ = cardtypes[ct]
            fields[typ['fields'][i]] = spl[i]
        card['fields'] = fields
        allcards.append(card)
    return (allcards,cardtypes)


if __name__ == '__main__':
    main()
    pass
"""
CREATE TABLE col (
    id              integer primary key,
    crt             integer not null,
    mod             integer not null,
    scm             integer not null,
    ver             integer not null,
    dty             integer not null,
    usn             integer not null,
    ls              integer not null,
    conf            text not null,
    models          text not null,
    decks           text not null,
    dconf           text not null,
    tags            text not null
)
CREATE TABLE notes (
    id              integer primary key,   /* 0 */
    guid            text not null,         /* 1 */
    mid             integer not null,      /* 2 */
    mod             integer not null,      /* 3 */
    usn             integer not null,      /* 4 */
    tags            text not null,         /* 5 */
    flds            text not null,         /* 6 */
    sfld            integer not null,      /* 7 */
    csum            integer not null,      /* 8 */
    flags           integer not null,      /* 9 */
    data            text not null          /* 10 */
)
CREATE TABLE cards (
    id              integer primary key,   /* 0 */
    nid             integer not null,      /* 1 */
    did             integer not null,      /* 2 */
    ord             integer not null,      /* 3 */
    mod             integer not null,      /* 4 */
    usn             integer not null,      /* 5 */
    type            integer not null,      /* 6 */
    queue           integer not null,      /* 7 */
    due             integer not null,      /* 8 */
    ivl             integer not null,      /* 9 */
    factor          integer not null,      /* 10 */
    reps            integer not null,      /* 11 */
    lapses          integer not null,      /* 12 */
    left            integer not null,      /* 13 */
    odue            integer not null,      /* 14 */
    odid            integer not null,      /* 15 */
    flags           integer not null,      /* 16 */
    data            text not null          /* 17 */
)
CREATE TABLE revlog (
    id              integer primary key,
    cid             integer not null,
    usn             integer not null,
    ease            integer not null,
    ivl             integer not null,
    lastIvl         integer not null,
    factor          integer not null,
    time            integer not null,
    type            integer not null
)
CREATE TABLE graves (
    usn             integer not null,
    oid             integer not null,
    type            integer not null
)
"""
