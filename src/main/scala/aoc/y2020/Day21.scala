package aoc.y2020

import scala.annotation.tailrec
import scala.collection.immutable

object Day21:
  def solve(input: String): Int =
    val food = input.linesIterator
      .map:
        case s"$ingredients (contains $understood)" => ingredients.split(' ').toList -> understood.split(", ").toList
      .toVector

    val allergens = food
      .flatMap((ing, all) => all.map(_ -> ing.toSet))
      .groupMapReduce(_(0))(_(1))(_ intersect _)
      .values
      .reduce(_ union _)

    food
      .flatMap((ing, _) => ing)
      .filter(!allergens.contains(_))
      .size

  def solve2(input: String): String =
    val food = input.linesIterator
      .map:
        case s"$ingredients (contains $understood)" =>
          ingredients.split(' ').toList -> understood.split(", ").toList
      .toVector

    @tailrec def simplify(allergens: Map[String, Set[String]]): Map[String, Set[String]] =
      val singles = allergens.collect:
        case (k, v) if v.size == 1 => k -> v.iterator.next
      val simplified = allergens.view
        .map((k, v) => k -> (v -- singles.filter((sk, _) => k != sk).values))
        .toMap
      if allergens == simplified then allergens
      else simplify(simplified)

    immutable.SortedMap
      .from(simplify(food.flatMap((ing, all) => all.map(_ -> ing.toSet)).groupMapReduce(_(0))(_(1))(_ intersect _)))
      .values
      .flatten
      .mkString(",")

  val input =
    """smfnh svztk rqqf sfhvsx xctnhp bvn krv gkcplx ngpq hmhll dq mqr vnvgrb xbp dqsbql dlx mgvr cqvpp nqfjgtsj zscj jbndg gz xrkckp srzsljf mzmph gbxk mvqkdj frkpm vnhn sjfcj rgfx fjbqn lvrsl dvvsn rgpbkp vhdzgz pvq ktlh pzvvlgt knpp sbpkr bbzn rrjjt tsfx brnsf gfxb qmcj fdfr glllxz cmkqcs fctjm fmrdsr xmrd bpdfhd cjxgg gszbq gtqsr ftds qrdtdv vjmfl phbhgqh nbhpx sjlk xhzj kmfj xdsxjv hcrz xzxth xxcq kpnl pbhvldj msfmt sttj fdrzd bprg jdbpd hbhsx dfllj frvs fltn xxgp vlhnhf ggsz dmxdkc qbrqsr cgbgf gbgf szp zrtfglg (contains nuts)
      |msfmt vcnrl tsfx dqsbql hcxhl nbhpx vjmfl fltn gfxb cmkqcs knpp lcqn srnbhq zrtr xg gspk gvnj mvqkdj vhchvg ggsz bpxggt kmfj gkcplx nlh xjggx jbndg fjbqn dq svztk lcrpq xhksjm sqzhc rgfx gbxk lttlx ssbrn xlkm fcps kkkt ppdplc lxkbs frkpm fjvf hbhsx hnxc (contains soy)
      |gz fjbqn xqxgc xbp lcrpq vnhn ftds gszbq qbrqsr nkmxp zjg hnxc hbhsx sjfcj svztk ggsz nvsd rgfx mzmph tllxsfs dvvsn mvqkdj brnsf rqlljq rqqf vbn jbndg tclx ppdplc lxkbs szp gkcplx sqzfbq sqxsf mqr fmrdsr ktv hcxhl jbmlk ktlh ppskx kkkt cbpx zsxfl cdrm tbhxh drnrd nx gspk nmdv kvgbhx fjvf xjggx dqsbql lvrsl ndtf zbjtv (contains fish, eggs)
      |gbgf xhksjm xg zscj nbhpx xzxth sqzfbq sfhvsx vnvgrb phbhgqh kctf tsfx fkfr pzvvlgt nqfjgtsj bhtxd qmcj hbhsx zbjtv jbndg xxgp smscxzt gfjqxx cbpx zsxfl tfd qbrqsr phbrht nmdv zgvqnlv ppdplc bxpdm xbsps rrjjt cqvpp zrtr xvrvnt vdnvf lttlx qjjxv mvqkdj ndtf prrpm tmkc tllxsfs cgbgf fjbqn hnxc frkpm gszbq rqlljq lhtqk ngqg ppskx knpp xqxgc fnmx msfmt gvnj pbhvldj kmfj rqqf hdmpps ktlh glllxz ssbrn zrtfglg hcrz dqsbql frvs cjcvh vnhn ggsz fdfr vhchvg tclx fjvf (contains soy)
      |sbpkr hbhsx gszbq lgxx hmhll tsfx cqvpp gkphjq nvsd tfd fzrfrs fvgpcjh dfllj drnrd xg hcxhl vjmfl mvqkdj ndtf dvvsn ngpq gkcplx fkfr xmrd lttlx qmcj dqsbql fdfr msfmt sqxsf sttj nkmxp nbhpx pvq cjxgg jbmlk xjggx xhzj fdrzd ktv kmfj vhchvg frkpm mqr rgfx mzmph srnbhq pbcptj zscj cgbgf sqzhc ppdplc xvrvnt ssbrn tmsn ktlh bprg cmkqcs lcqn cdvdt lvrsl rgpbkp smscxzt cdrm tclx xzxth xbsps xlkm srzsljf tmkc zrtfglg gvnj krv gxgbkd dfkt npjrb xbp vdnvf zgvqnlv pbhvldj (contains dairy, fish, eggs)
      |vdnvf kkxq sttj mhzdcc srnbhq krtlhc fjbqn fjvf mvvbb vhchvg lgxx cqvpp lxkbs ggsz xjggx jbndg rqqf qrdtdv lttlx bhtxd xdsxjv nbhpx mvqkdj dfllj ndtf hcrz gtqsr ftds fnmx rrjjt fvgpcjh szp tmkc brnsf vbn frxfc zscj bxpdm dqsbql xhzj bpdfhd kddggl fhhq sjlk prrpm hdmpps msfmt gbxk phbhgqh xmrd vpsv zsxfl xzxth xvrvnt gkcplx kctf xxgp xg fzrfrs fspk kvgbhx cjcvh lcqn ppskx zrtfglg drnrd dfkt hnxc fdfr rqlljq tclx gszbq cbpx hbhsx gspk ngpq bpxggt ktlh gbgf xqxgc kxmgm (contains wheat, fish, soy)
      |fdfr vpsv kkkt kctf kkxq drnrd prrpm kmfj gz qjjxv vhdzgz dqsbql bxpdm fqrtvrv qbrqsr dfllj cdrm vlhnhf tclx ktlh sjlk lhtqk vhchvg pzvvlgt pvjd lttlx tllxsfs sqzhc bvn xdsxjv mzmph cmkqcs xxcq blbl sqzfbq frkpm ggsz fkfr jdbpd cqvpp zjg vjmfl gbxk mvvbb mhzdcc ngqg ndtf llhld cjxgg pbcptj fmrdsr ppdplc msfmt dlx zrtfglg mvqkdj ccxlqzv gkcplx brjx fspk fvgpcjh jsmpg pvq fdrzd smscxzt xvrvnt rgpbkp gtqsr lpgptlj hdmpps fzrfrs zgvqnlv (contains shellfish)
      |gspk vdnvf lcqn fqxcjh jdbpd tsfx kkxq hmhll vhchvg gxgbkd xzxth krtlhc krv jbndg fdfr ggsz dqsbql xqxgc fkfr mgvr zscj sqzhc svztk gkphjq vpsv glllxz srnbhq ngqg gkcplx kvgbhx cbpx zrtr hpqf hdmpps cmkqcs ppdplc srzsljf gcch sqzfbq fctjm pvjd qfktb cdrm cgbgf xbp cjxgg kkkt lttlx vnvgrb xxgp hrbbhg msfmt mvqkdj dq gfjqxx ktv hcxhl tbhxh drnrd nmdv hcrz ktlh (contains dairy, eggs, sesame)
      |sqzhc vdnvf krv nkmxp pvq dq fvgpcjh dfkt xmrd fjvf vpsv xdsxjv zrtr zjg xhzj zgvqnlv nbhpx fnmx tfd ppdplc xhksjm ktlh pzvvlgt msfmt xg dqsbql hrbbhg gszbq cjcvh rqqf mzq ndtf gtqsr cdvdt mvqkdj vnvgrb hpqf lpgptlj gkcplx srzsljf lcrpq jbndg gcch fltn gbgf fhhq hbhsx fqrtvrv xbsps gbxk vjmfl xbp hcxhl llhld rggl zsxfl kvgbhx frxfc xxgp (contains sesame, wheat)
      |dnkcf fqrtvrv cmkqcs ktlh pbhvldj blbl nx xmrd vlhnhf rqqf gkcplx hcxhl nmdv brjx tllxsfs hbhsx frxfc hdmpps qfktb lcrpq mvqkdj smscxzt qjjxv zsxfl frkpm lttlx ppdplc vhdzgz fctjm ndtf kkxq gkphjq ggsz ktv xctnhp fkfr zscj dqsbql xzxth xbsps brnsf tclx tbhxh jsmpg vjmfl xxcq lgxx kvgbhx llhld (contains shellfish, sesame, fish)
      |zrtr frxfc mvqkdj zrtfglg nx hbhsx phbrht nlh brjx ssbrn pvq sqxsf jbmlk jzjb jshns nkmxp fdrzd hcxhl pgtrjst mgvr qjjxv ggsz xlkm smfnh srzsljf vnhn fctjm rqlljq fqrtvrv lttlx bpdfhd sttj nmdv dfllj knpp ktlh frkpm zbjtv tllxsfs fjbqn fvgpcjh zscj fjvf vjmfl xxgp xg msfmt fnmx llhld vdnvf phbhgqh mhzdcc xzxth sqzhc xmrd hcrz xctnhp ktv vlhnhf gkcplx qvxntmh dqsbql tbhxh gcch hdmpps ngqg glllxz fmrdsr sfhvsx dmxdkc (contains eggs, soy)
      |ccxlqzv ngpq qmcj hcrz nkmxp fspk rxtp gcch ktv vcnrl rgfx llhld sqzhc jbndg ssbrn pzvvlgt prrpm xqxgc rkgb lpgptlj srzsljf fltn srnbhq fjvf mqr krv dfkt fdfr glllxz jdbpd npjrb ktlh dnkcf pvjd hbhsx cjcvh dmxdkc xrkckp qfktb rqlljq dqsbql ggsz tmsn phbhgqh kctf mvqkdj szp gkcplx gbxk qvxntmh fnmx vnvgrb nqfjgtsj gvnj tfd nlh ppdplc pvq bbzn lttlx bpxggt lgxx xxgp hdmpps cjxgg gfxb kxmgm knpp brjx (contains wheat)
      |nkmxp hbhsx frkpm tfd vlhnhf mgvr fdfr mqr zjg tsfx xdsxjv vnvgrb fqxcjh vcnrl llhld lpgptlj sfhvsx rgpbkp fzrfrs xrkckp qmcj bvn gkphjq lcqn pvq xhksjm smfnh ggsz vbn gfxb cqvpp bxpdm xmrd fcps pbhvldj msfmt qfktb mzq rggl fltn ppdplc fjbqn xzxth ppskx gfjqxx fdrzd nx gvnj ktlh srnbhq xbp fnmx qsx jbmlk hmhll vhchvg xbsps krv dqsbql sqzfbq xctnhp gkcplx lxkbs ssbrn szp pvjd blbl fhhq (contains wheat)
      |fltn frxfc qrdtdv gkcplx jdbpd phbhgqh mhzdcc dfllj mvqkdj sfhvsx rggl zrtfglg nqfjgtsj gszbq hcrz sqxsf fzrfrs kpnl ppdplc pbhvldj llhld fjbqn kkxq ssbrn qsx phbrht gvnj cjcvh fcps fqrtvrv xctnhp hbhsx zscj smscxzt dlx fjvf msfmt vjmfl xvrvnt dqsbql gcch ndtf rxtp kmfj gspk dnkcf sjfcj lcqn ggsz zsxfl jshns (contains nuts)
      |qfktb fqxcjh cjcvh cdvdt hcxhl mvvbb hcrz glllxz nqfjgtsj sqzhc nbhpx zjg xmrd pgtrjst tmkc vnvgrb cgbgf lttlx ppdplc sjfcj dvvsn gszbq dmxdkc kxmgm dq brnsf fjbqn vdnvf qrdtdv ggsz vcnrl cbpx phbhgqh pvq jsmpg sttj mvqkdj xbsps sqxsf ktlh xdsxjv bpdfhd vhchvg tfd lvrsl xzxth lcqn qbrqsr hpqf nlh krtlhc lgxx zbjtv ccxlqzv rxtp zrtfglg nmdv ftds msfmt xhksjm gkcplx dqsbql mzmph tbhxh srzsljf jzjb cmkqcs gbgf dlx srnbhq lpgptlj rrjjt (contains shellfish, eggs)
      |dqsbql lcrpq ftds hbhsx brjx fqxcjh llhld msfmt drnrd gfxb xbsps nvsd gkcplx xvrvnt zscj gtqsr tfd qsx ngqg fqrtvrv phbhgqh dnkcf gz xg qjjxv sqzfbq jdbpd sbpkr mzmph ssbrn hmhll cqvpp xqxgc cjxgg krtlhc smscxzt gxgbkd mvqkdj xdsxjv brnsf vhchvg lcqn kxmgm qvxntmh pbhvldj ktlh cdvdt dq szp ppdplc dfllj fvgpcjh fdrzd knpp kpnl tmsn nqfjgtsj rggl jsmpg xzxth cdrm bpxggt vjmfl cgbgf rkgb xhksjm nlh dfkt sqxsf (contains fish, eggs)
      |xrkckp nlh ktlh gfxb jbndg zscj rqlljq xctnhp nkmxp vhdzgz gfjqxx mgvr fdfr sqxsf bxpdm gkcplx fmrdsr msfmt smscxzt krv fqxcjh kpnl cgbgf npjrb prrpm qbrqsr ppskx gxgbkd mvvbb ggsz gcch ppdplc gszbq lttlx fspk dqsbql dfkt bpxggt szp rggl fltn tsfx bvn fnmx lpgptlj mzmph svztk gbgf mqr xg lcqn mvqkdj pgtrjst phbrht fhhq cbpx glllxz qjjxv kctf brnsf drnrd sjlk kkkt (contains sesame, shellfish, dairy)
      |ngqg mvqkdj gkphjq dlx dqsbql ktlh rqlljq bpxggt vnhn srnbhq frxfc fspk frvs kctf lxkbs jzjb glllxz rgfx pbcptj fnmx xxcq bprg ppskx rqqf qvxntmh cmkqcs rggl jdbpd fjbqn frkpm mzq hpqf sqzfbq llhld xqxgc vnvgrb tllxsfs zscj lvrsl ngpq vhdzgz nlh ppdplc gz zgvqnlv bvn gspk jsmpg fzrfrs fkfr rrjjt msfmt cjxgg krv vhchvg ggsz npjrb smscxzt gtqsr kddggl hbhsx kpnl xg sbpkr tmsn nkmxp xjggx nbhpx jshns cjcvh (contains shellfish)
      |ppdplc dqsbql brjx ktlh xrkckp kpnl zrtfglg svztk dfkt mhzdcc xmrd kddggl sjlk nkmxp jshns xvrvnt zbjtv xdsxjv mvqkdj glllxz vlhnhf hbhsx hrbbhg fdrzd xg pbcptj xbp rkgb ngpq msfmt lttlx fdfr llhld dmxdkc krv cdvdt rggl qjjxv xxgp vbn sbpkr gvnj vnvgrb pbhvldj vjmfl dq srnbhq mvvbb gkcplx vnhn bvn jbmlk xzxth krtlhc fjvf kxmgm dvvsn vpsv rxtp cgbgf nmdv (contains nuts)
      |pgtrjst ktlh fmrdsr llhld pbhvldj dfllj lvrsl xqxgc ppdplc rqqf lcrpq nlh vnhn cdvdt msfmt hmhll zbjtv ktv fspk tclx fdfr prrpm lgxx sbpkr dfkt qrdtdv sjfcj vhdzgz dlx cqvpp xhzj vhchvg tllxsfs fctjm mvqkdj bbzn qjjxv vjmfl sttj ndtf nkmxp tsfx vnvgrb sjlk dqsbql ggsz sqxsf fjbqn qbrqsr vbn dnkcf gfxb lxkbs cmkqcs hbhsx kxmgm zgvqnlv vlhnhf xmrd gszbq srnbhq gxgbkd xhksjm xrkckp mvvbb gtqsr ccxlqzv jshns tmkc jbndg frxfc tfd zrtr fzrfrs (contains nuts, shellfish)
      |nmdv svztk vpsv zsxfl gfxb fnmx pzvvlgt dlx rkgb tclx fdfr rqlljq lvrsl sfhvsx blbl kvgbhx jsmpg tfd xrkckp kkkt gkcplx cdvdt ppdplc fqxcjh brjx tmsn dq kxmgm bhtxd xvrvnt fspk sjfcj gcch xxgp hpqf mgvr srnbhq qvxntmh xzxth msfmt jbmlk dqsbql srzsljf ktlh zbjtv gz mvqkdj nkmxp rrjjt xqxgc cjcvh kctf mzq dfllj hbhsx fmrdsr fdrzd bpxggt kmfj gkphjq (contains wheat)
      |rgfx fvgpcjh mzmph cqvpp ggsz gfjqxx kctf gspk zrtfglg gkphjq dmxdkc ktlh fdfr qfktb srnbhq xctnhp phbhgqh zgvqnlv xqxgc rqqf sfhvsx xbsps cjcvh kkkt nlh fhhq cdrm kddggl mvqkdj npjrb lhtqk hdmpps zbjtv nmdv lttlx vbn pzvvlgt lcrpq dvvsn cdvdt bpdfhd nkmxp hrbbhg ssbrn rggl dfllj vhchvg pbcptj dnkcf xjggx nbhpx fdrzd vhdzgz pbhvldj mvvbb qjjxv zscj hbhsx gxgbkd vnhn gz ppdplc cmkqcs hcrz bprg prrpm vnvgrb msfmt pvq gkcplx rkgb bxpdm (contains wheat, shellfish, soy)
      |nkmxp xg vjmfl gspk gxgbkd ggsz cmkqcs fjbqn mvqkdj mgvr jdbpd hbhsx cjcvh zbjtv xxcq xqxgc gkcplx zgvqnlv gtqsr lgxx gfxb msfmt qjjxv pbcptj bpdfhd bvn xrkckp fzrfrs pgtrjst xmrd vpsv nx gz dfkt fcps xhksjm nlh bprg vhdzgz drnrd ngqg gkphjq dfllj rgpbkp fkfr fqxcjh xzxth kctf tbhxh mvvbb dq hpqf mzmph xctnhp dmxdkc lvrsl tmsn lxkbs fdrzd rxtp cgbgf nvsd gbgf krv rggl frvs gvnj vcnrl ppdplc sttj hrbbhg gszbq qmcj hcxhl fspk fnmx dqsbql nqfjgtsj kxmgm ccxlqzv zjg (contains nuts, sesame, eggs)
      |npjrb xvrvnt sttj pvjd lvrsl pgtrjst fjbqn lcqn xxgp kvgbhx qvxntmh bvn cbpx gkphjq gkcplx gszbq rxtp mzmph lgxx zrtfglg gspk gbxk nvsd bpdfhd sjlk kctf qsx kddggl vhdzgz ktv xrkckp fqrtvrv jdbpd fltn fctjm frxfc svztk nx phbhgqh vpsv kxmgm hmhll ktlh xdsxjv phbrht zgvqnlv smfnh tbhxh sbpkr srzsljf xjggx mhzdcc xhzj fdrzd ccxlqzv cdrm dlx msfmt mgvr ppdplc qmcj lhtqk hcxhl gbgf fspk ngpq lpgptlj ftds jzjb cmkqcs ggsz rqqf mvqkdj cdvdt brnsf xzxth smscxzt fdfr tmsn ndtf brjx gfxb rrjjt sjfcj vhchvg dqsbql cqvpp qrdtdv (contains soy, dairy)
      |lcrpq fmrdsr fdfr gtqsr nlh hnxc bprg dqsbql hmhll mvqkdj hbhsx sqxsf fdrzd szp bbzn pbcptj sbpkr qsx rgpbkp xctnhp rxtp bvn ktlh bxpdm zgvqnlv lhtqk bpdfhd sjfcj xdsxjv nmdv qjjxv ppdplc ggsz qfktb vhdzgz vlhnhf xhksjm nqfjgtsj vnvgrb mhzdcc msfmt jsmpg dfkt (contains fish)
      |ppdplc rqqf vbn zjg xdsxjv msfmt nlh nkmxp mhzdcc glllxz bvn xjggx gfxb zrtfglg qvxntmh sqzfbq qfktb gz bpxggt fqxcjh phbhgqh bhtxd frkpm rqlljq dmxdkc lcqn vlhnhf vdnvf mzq sfhvsx svztk qbrqsr hnxc hbhsx brjx mvqkdj dq ktlh dqsbql dvvsn fzrfrs mvvbb tmkc vcnrl blbl gkcplx vhchvg (contains sesame)
      |rrjjt zsxfl dqsbql dmxdkc vcnrl vhchvg vnhn mzmph ssbrn rgpbkp msfmt fvgpcjh lpgptlj gbgf xmrd frxfc ktv ggsz nqfjgtsj qjjxv xzxth lhtqk qfktb cdvdt ktlh xbsps ftds phbrht jsmpg jbndg zjg mvqkdj sqxsf brnsf ccxlqzv zrtr pvq bpdfhd frvs ppdplc vjmfl gbxk blbl kvgbhx sjfcj mvvbb xg gz kmfj xdsxjv nkmxp svztk lvrsl hbhsx bxpdm xxgp vpsv hcxhl (contains nuts, soy, wheat)
      |nkmxp hnxc rqlljq knpp vnhn zjg rqqf rrjjt xlkm msfmt xbsps jshns vhdzgz gfxb brnsf qmcj fspk dmxdkc xzxth lcrpq phbhgqh ggsz bpxggt szp lxkbs sqxsf gszbq brjx ngqg qrdtdv dqsbql gkphjq bpdfhd ktlh mgvr ktv mvqkdj xctnhp ppdplc ndtf nqfjgtsj pzvvlgt jbmlk dlx lgxx rgfx fjvf gkcplx xhksjm kmfj llhld (contains fish)
      |gvnj kkxq ktlh gkphjq tsfx svztk msfmt pvq lpgptlj gz gbgf fzrfrs hcxhl dnkcf xbp gfxb tllxsfs kpnl fjbqn zscj ppdplc lxkbs lcqn hcrz zjg kddggl gkcplx ngqg vnvgrb hbhsx hmhll rxtp bpdfhd frxfc fqxcjh pgtrjst qfktb fhhq xbsps xhzj srnbhq bhtxd vjmfl ggsz ktv phbhgqh fkfr dqsbql vbn jsmpg rgfx xrkckp sjfcj llhld nbhpx phbrht rggl tbhxh blbl vcnrl rgpbkp nqfjgtsj lvrsl dq nvsd mvvbb ngpq xxgp nkmxp xmrd gxgbkd fltn jbmlk rqlljq qvxntmh (contains dairy, sesame, eggs)
      |nbhpx gcch fvgpcjh gtqsr srzsljf cgbgf nmdv cjxgg tsfx ngpq vbn rkgb dnkcf nqfjgtsj vnhn xqxgc ngqg dmxdkc vhdzgz fzrfrs glllxz kddggl dqsbql vhchvg xbp svztk mhzdcc ppdplc mzq qvxntmh phbhgqh frxfc pvq lxkbs bxpdm sqxsf kkxq gkcplx zrtr fnmx bvn kkkt pzvvlgt gz fdfr gfxb xhzj fltn vjmfl qsx sfhvsx hbhsx sttj ftds qmcj tmsn rgfx mzmph gfjqxx jbmlk qjjxv kxmgm fcps vnvgrb ggsz mvqkdj sjfcj nkmxp pbhvldj ktlh (contains eggs, dairy, fish)
      |gkcplx xrkckp vbn smscxzt mhzdcc cjxgg kddggl zgvqnlv dqsbql ppdplc kkkt hbhsx gspk kvgbhx rqqf prrpm xxcq mzq srzsljf msfmt xbsps hpqf ngpq pbcptj mzmph xmrd gvnj ktlh cmkqcs dlx cbpx hmhll xlkm bhtxd mvqkdj gbxk nx (contains shellfish, fish)
      |qsx zjg lcrpq glllxz zscj rggl dmxdkc jshns frvs sjfcj kddggl xlkm msfmt rxtp zsxfl ktlh kpnl frkpm sttj ngpq brjx brnsf hcrz fctjm fcps zbjtv nx cmkqcs mhzdcc kkxq vpsv pbcptj qrdtdv fltn llhld gkcplx mvvbb qmcj nkmxp szp rqqf dvvsn nvsd jzjb mzmph dlx gbgf mvqkdj dqsbql vjmfl gszbq phbhgqh ppdplc tbhxh xxgp fvgpcjh ccxlqzv ggsz lhtqk vcnrl bxpdm gtqsr jbmlk srzsljf bbzn lgxx rqlljq frxfc zrtfglg (contains dairy)
      |ppdplc dmxdkc gtqsr fdfr srnbhq hnxc xvrvnt jbndg lxkbs bhtxd fzrfrs vpsv pzvvlgt dqsbql zrtfglg frkpm cqvpp zjg npjrb fvgpcjh vnvgrb fnmx pvjd fdrzd frxfc nmdv zbjtv tclx bxpdm phbhgqh krv ktv qrdtdv knpp sttj hbhsx sqxsf xqxgc vhdzgz gkcplx lcqn rrjjt tmkc xxgp rqlljq bbzn gszbq jzjb zscj fjvf ktlh vlhnhf mgvr xbp smfnh dlx rgpbkp lgxx msfmt lcrpq hcrz ggsz xhzj qvxntmh gxgbkd vnhn cgbgf xbsps xjggx gspk jshns ftds cdvdt rggl pvq xhksjm ndtf kkxq gz pbhvldj fmrdsr mzmph gfjqxx (contains fish, eggs, soy)
      |kctf bxpdm tmsn cgbgf kpnl vnvgrb rggl bpxggt kddggl pvq msfmt xvrvnt lxkbs xxgp npjrb knpp svztk gz fvgpcjh ndtf fdrzd sqzhc gfxb fqxcjh sbpkr gbxk hcrz zsxfl zgvqnlv nx blbl llhld sjfcj rqqf cdrm ggsz xqxgc ftds sfhvsx fctjm krtlhc srzsljf jdbpd mvvbb dqsbql nvsd dmxdkc lcqn gkcplx fjbqn glllxz ktlh vhchvg ssbrn gxgbkd dvvsn kxmgm pbcptj xbp tmkc hbhsx mvqkdj (contains wheat, nuts, shellfish)
      |fmrdsr gtqsr sqxsf fvgpcjh lpgptlj mvqkdj frvs nkmxp zrtr bxpdm ccxlqzv msfmt vjmfl dvvsn hnxc zgvqnlv nqfjgtsj mzmph rrjjt gkcplx pgtrjst mvvbb vbn tllxsfs bhtxd nlh qsx vpsv npjrb jbndg gxgbkd tclx xhksjm sjfcj ktv rxtp hcxhl xzxth qjjxv hbhsx ggsz xxgp tmsn ktlh dqsbql dlx kctf gszbq bbzn pzvvlgt gbgf cqvpp fqrtvrv szp pbhvldj (contains eggs, sesame)
      |xhksjm mhzdcc kvgbhx ngpq rkgb nlh ppdplc qbrqsr gbxk ngqg drnrd phbrht zrtr dq rgfx qvxntmh dnkcf phbhgqh mvqkdj rrjjt fjbqn nkmxp xdsxjv cjxgg ktlh kctf xmrd zgvqnlv blbl msfmt fdfr cjcvh tsfx sjlk lgxx vhchvg ggsz sqzfbq svztk lhtqk zsxfl nvsd vhdzgz gkcplx ssbrn lcqn rqqf kmfj vcnrl mvvbb hbhsx pbhvldj gspk tmkc hpqf kddggl nmdv xbp cdvdt ktv xctnhp (contains eggs)""".stripMargin
