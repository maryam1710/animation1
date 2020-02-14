# animation1
<div dir="rtl">
 در این بخش روش های مختلفی را جهت ایجاد نمودارهای متحرک با استفاده از محیط R بیان خواهیم کرد.انیمیشن  عنصر بسیار مهم تجسم داده 
 است. نمودارهای متحرک از نظر بصری جذاب هستند و توجه مخاطبان را به خود جلب میکنند. نمودارهای متحرک میتوانند برای نشان دادن مفاهیم یا کشف روابط بسیار مفید باشند و همین امر سبب میگردد که این دست از نمودارها در آموزش و تحقیقات اکتشافی بسیار مفید باشند.
خوشبختانه ایجاد نمودارهای متحرک در محیط آر کاملا ساده است، کافی است چند ابزار مناسب را در اختیار داشته باشیم و چند اصل اساسی در مورد نحوه ایجاد تصاویر متحرک را درک کنیم. 

بسیاری از ابزارهای تجسم داده آنلاین در بازار در دسترس هستند که می توانند نمودارهای متحرک ایجاد کنند اما بیشتر این ابزارها پرداختی هستند، همچنین مشکل ابزارهای انیمیشن آنلاین این است که از شما می خواهند اگر بر روی داده های واقعی میخواهید کار کنید، داده ها را در سرور آنها بارگذاری کنید.

از آن جهت که محیط R متن باز  است، میتوانیم آن را به صورت رایگان بارگیری کنیم و بدون انتقال اطلاعات به سرور، نمودارهای متحرک ایجاد کنیم.

یکی از روشهایی که میتوانیم در محیط R نمودارهای متحرک ایجاد کنیم، استفاده از بسته
{gganimate} است.
دیوید رابینسون  در اوایل سال 2016 اقدام به ساخت بسته
{gganimate} نمود.
دیدگاه رابینسون تنظیم قاب  به عنوان یک عنصر زیبایی بود، که درک آسان این دیدگاه سبب موفقیت آن شد. نسخه ساخته شده توسط رابینسون هرگز وارد مجموعه {CRAN} نشد.

هر تصویر از دو بخش اصلی تشکیل شده است:
<p>
<menu>
 <p><menuitem>چیزی که ما به آن نگاه میکنیم</menuitem></p>
<p> <menuitem>هدف و مفهومی که در آن تصویر گنجانده شده است</menuitem></p>
</menu>
</p>
مورد اول توسط انتقال  و سایه‌ها  انجام می‌گیرد، در حالی که مورد دوم توسط دید  اداره می‌گردد. به طور خلاصه:
<p>
<menu>
 <b>انتقال</b>
 <menuitem>
قاب‌های انیمیشن را بر اساس داده‌های اختصاص یافته به هر لایه پر می‌کند.
 </menuitem>
 </p>
 <p>
<b>سایه‌ها</b>
<menuitem>
با اجازه دادن به هر قاب، داده‌هایی از قاب‌های قبلی یا آینده را به هر قاب دیگری می‌دهند.
</menuitem>
</p>
<b>دیدها</b>
<menuitem>
به ما این امکان را می‌دهند تا دامنه مقیاس‌های موقعیتی (بزرگنمایی )  را مستقیم یا به عنوان تابعی از داده‌های اختصاص یافته به قاب، تغییر دهیم.
</menuitem>
</menu>
</p>
در این بخش قصد داریم نمونه‌هایی از نحوه استفاده از بسته {gganimate} را جهت ایجاد نمودار متحرک در محیط {R} ارائه دهیم. ما قصد داریم از این بسته جهت تولید یک نمودار متحرک با فرمت {gif} در محیط {R} استفاده کنیم.
\example
می‌خواهیم با یک مثال ساده شروع کنیم، در همین راستا ما قصد داریم تعدادی داده ساختگی را جهت تصویر سازی ایجاد کنیم. در برنامه \eqref{c21} ما سه ستون حاوی مشاهدات تصادفی تولید می‌کنیم. ستون اول به نام {A}، شامل 50 مشاهده از 1 تا 75 است، به همین ترتیب ستون دوم شامل تعداد مشابه مشاهدات است اما فاصله دامنه مشاهدات متفاوت است.
<div>
<p>
'> df = data.frame(A=sample(1:75, 50, replace=TRUE),'</p>
<p>'+ B=sample(1:100, 50, replace=TRUE),'</p>
<p>'+ stringsAsFactors = FALSE) '</p> 
 </div>
گاهی اوقات ممکن است کاربر نتواند تفاوتی بین دستور زبان  و {API} قائل شود و تصور کند هر دو یکی هستند، در صورتی که این چنین نیست. دستور زبان یک ساختار تئوری و یک پایه و اساس است که بر اساس آن می‌توان \lr{API} را تعریف کرد. چندین \lr{API} می‌توانند یک دستور زبان را به چندین روش متفاوت اجرا کنند. ما سعی کردیم تا حد امکان {API gganimate} را با {API ggplot2} هم تراز کنیم، به نحوی که اختلاف بین دو بسته محو شود. ما با اضافه کردن توابعی از بسته {gganimate} به نمودار خود، می‌توانیم آن را به یک نمودار متحرک تبدیل کنیم و هنگام چاپ تصویر متحرک به همان روشی که {ggplot} هنگام چاپ تصویر ارائه می‌شود، انیمیشن نیز ارائه می‌شود. نمونه‌ای از این روش اضافه نمودن تابع{transition-reveal()} به نمودار است به جهت اینکه نمودار به تدریج در طول یک متغیرعددی ظاهر شود، (شکل \eqref{n6}).
\vspace{.5cm}

\begin{latin}
\begin{Rcode}
> library(ggplot2)
> library(gganimate)
> ggplot(df, aes(A, B)) +
+     geom_line() +
+     transition_reveal(A) +
+     labs(title = 'A: {frame_along}')
\end{Rcode}
\end{latin}
\captionof{lstlisting}{اضافه نمودن تابع {transition-reveal()} به این دلیل که نمودار به تدریج نمایان شود.}
\label{c22}

\begin{center}
\includegraphics[scale = .7]{transi}
\end{center}
\captionof{figure}{در این تصویر یک نمودار متحرک مشاهده می‌کنید که توسط استفاده از بسته {gganimate} ترسیم شده است و با فرمت \lr{gif}ذخیره شده است.}
\label{n6}

آرگومان {frame-along} موقعیتی را به ما نشان می‌دهد که قاب فعلی با آن مطابقت دارد، ({A:41}).
\footnote{اتفسیر} در انیمیشن بسیار مهم است. در انیمیشن، یک قاب از تعداد زیادی تصاویر متحرک
شناخت دو اصطلاح {frame}  و {rendering}  تشکیل شده است که در نهایت این تصاویر سبب ایجاد نمودار متحرک می‌شوند. تفسیر یک نوع محاسبه برای نتیجه نهایی است. در بسته {gganimate}، به صورت پیش‌فرض 100 قاب جهت تفسیر وجود دارد، ما می‌توانیم تعداد قاب‌ها را با استفاده پارامتر {nframes} در تابع {animate} تنظیم کنیم، (برنامه \eqref{c23}).

\begin{latin}
\begin{Rcode}
> library(ggplot2)
> library(gganimate)
> p = ggplot(df, aes(A, B)) +
+     geom_line() +
+     transition_reveal(A) +
+     labs(title = 'A: {frame_along}')
> animate(p, nframes = 40)
\end{Rcode}
\end{latin}
\captionof{lstlisting}{تغییر تعداد قاب‌ها با استفاده از پارامتر {nframes} که در تابع {animate} قرار دارد.}
\label{c23}

ما می‌توانیم میزان زمان صرف شده برای هر قاب در ثانیه را که به طور پیش‌فرض 10 قاب در ثانیه است را توسط پارامتر {fps} در تابع {animate()} کنترل کنیم، (برنامه \eqref{c24}).


\begin{latin}
\begin{Rcode}
> animate(p, nframes = 40, fps = 2)
\end{Rcode}
\end{latin}
\captionof{lstlisting}{نحوه استفاده از پارامتر {fps} جهت کنترل زمان صرف شده برای هر قاب در ثانیه.}
\label{c24}


\textbf{توجه}:
کاهش میزان قاب در ثانیه از کمتر از 10، به معنای کاهش سرعت انیمیشن است.

پس از ترسیم نمودار پویا در محیط {R}، تصویر متحرک ایجاد شده بارها تکرار می‌شود، برای پایان دادن به این حلقه تکرار می‌توانیم از پارامتر {renderer = gifski-renderer(loop = )} که در تابع {animate()} قرار دارد استفاده کنیم و مقدار {loop} برابر با مقدار منطقی {FALSE} قرار دهیم. در ادامه مثال همچنین می‌توانیم اندازه ارتفاع و عرض نمودار خود را در اندازه دلخواه تغییر دهیم، (برنامه \eqref{c25} و شکل \eqref{n7} را ببینید).



\begin{latin}
\begin{Rcode}
> animate(p, fps = 10, duration = 14, width = 800, height = 400)
\end{Rcode}
\end{latin}
\captionof{lstlisting}{کد مربوط به تغییر اندازه عرض و ارتفاع نمودار با استفاده از پارامترهای {width} و {height}.}
\label{c25}


\begin{center}
\includegraphics[scale = .7]{transi1}
\end{center}
\captionof{figure}{در این تصویر یک نمودار متحرک را مشاهده می‌کنید، که ارتفاع و عرض آن توسط دو پارامتر {width} و {height} در تابع {animate}.}
\label{n7}


\example
در مثال دوم قصد داریم با استفاده از نمودار پراکندگی متحرک و داده‌های {Gapminder}، رابطه بین امید به زندگی  و سرانه تولید ناخالص داخلی پنج قاره را بین سال‌های 1952 تا 2007 مورد بررسی قرار دهیم و هر قاره را در یک نمودار مجزا ترسیم کنیم.


\begin{latin}
\begin{Rcode}
> library(gapminder)
> ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
+   geom_point(alpha = 0.7, show.legend = FALSE) +
+   scale_colour_manual(values = country_colors) +
+   scale_size(range = c(2, 12)) +
+   scale_x_log10() +
+   facet_wrap(~continent) +
+   labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
+   transition_time(year) +
+   ease_aes('linear')
\end{Rcode}
\end{latin}
\captionof{lstlisting}{نحوه ترسیم نمودار پراکندگی متحرک با استفاده از داده‌های {gapminder}.}
\label{c26}
\vspace{.7cm}

همانطور که در برنامه \eqref{c26} مشاهده می‌کنید ابتدا بایستی با استفاده از تابع {ggplot} و پارامتر {geom-point} یک نمودار پراکندگی ایجاد کنیم، از آنجایی که هر نقطه از نمودار نشان دهنده یک کشور است پس به کمک گزینه {values} در پارامتر \lr{scale-colour-manual} مشخص می‌کنیم که رنگ هر نقطه از نمودار با توجه به رنگ کشور مربوطه مشخص گردد. با توجه به اینکه بیشتر نمایشگرها تقریبا مستطیل شکل هستند، پارامتر {facet-wrap} به ما این امکان را می‌دهد که ابعاد صفحه نمایش و نحوه نمایش نمودارها را با توجه به تعداد قاره‌ها تنظیم کنیم.

ما در برنامه \eqref{c26} از تابع {transition-time()} که از توابع بسته {gganimate} است، و برای متغیرهای پیوسته مانند "سال" به‌کار گرفته می‌شود، استفاده کرده‌ایم. در تابع {transition} نیاز نیست جهت انتقال و طول نقاط را مشخص کنیم، زیرا "متغیر انتقال " مستقیما این کار را انجام می‌دهد، (به عنوان مثال انتقال بین سال‌های 1980‌ تا 1990 باید دو برابر طولانی‌تر از انتقال بین سال‌های 2000 تا 2005 طول بکشد).

زمانی که از تابع {transition-states()} استفاده می‌کنیم باید تصمیم بگیریم که داده‌ها چگونه انتقال پیدا کنند. این قابلیت به کمک تابع {ease-aes()} امکان پذیر است. پیش‌فرض این تابع مقدار {linear} است، به این معنا است که داده‌ها به صورت خطی انتقال پیدا می‌کنند.
\vspace{.7cm}

\textbf{توجه}:
دقت داشته باشید که پیش از فراخوانی بسته {gapminder} ابتدا بایستی دو بسته {usethis} و {devtools} را فراخوانی نمود.

\begin{center}
\includegraphics[scale = .9]{gap}
\end{center}
\captionof{figure}{در این تصویر یک نمودار متحرک را مشاهده می‌کنید، که امید به زندگی و سرانه تولید ناخالص دولتی پنج قاره را بین سال‌های 1952 تا 2007 مورد بررسی قرار داده است، و به هر قاره یک نمودار را اختصاص داده است. هر دایره یک کشور را نشان می‌دهد، مرکز دایره مربوط به (درآمد، سرانه تولید ناخالص داخلی) و منطقه متناسب با اندازه جمعیت است.}
\label{n8}


\example
در این مثال می‌خواهیم از مجموعه داده‌های گل زنبق استفاده کنیم، و برخی از آرگومان‌ها و توابع بسته {gganimate} را جهت ترسیم نمودارهای متحرک، تشریح کنیم. این مجموعه داده چند متغیره است که توسط رانلد فیشر  در سال 1936 معرفی شده است و مجموعه داده زنبق اندرسون نیز نامیده می‌شود. این مجموعه داده مربوط به 150 نمونه از 3 نوع گل زنبق  هستند که از هر نوع 50 عدد وجود دارد، هر نمونه شامل 4 ویژگی می‌باشد. این 4 ویژگی به ترتیب شامل طول کاسبرگ ، پهنای کاسبرگ ، طول گلبرگ  و پهنای گلبرگ هستند.

ابتدا ما با یک نمودار ایستا شروع می‌کنیم به این صورت که با استفاده از بسته {ggplot2} و تابع {ggplot()} یک نمودار نقطه‌ای ترسیم می‌کنیم و سپس با استفاده از تابع {transition-states()} به نمودار خود تحرک می‌بخشیم، (برنامه \eqref{c27}).


\begin{latin}
\begin{Rcode}
> library(ggplot2)
> p = ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
+   geom_point()
> plot(p)
\end{Rcode}
\end{latin}
\captionof{lstlisting}{استفاده از تابع {ggplot()} جهت ترسیم یک نمودار ایستا. در این نمودار طول و پهنای  گلبرگ 150 نمونه گل زنبق را با هم مقایسه کرده‌ایم.}
\label{c27}
\vspace{.5cm}

\begin{center}
\includegraphics[scale = .7]{trans1}
\end{center}
\captionof{figure}{}
\label{n9}
حال می‌توانیم با اضافه کردن توابعی از بسته {gganimate}، به برنامه \eqref{c27}، نمودار ایستا ترسیم شده را به یک نمودار متحرک تبدیل کنیم، (شکل \eqref{n10} و برنامه \eqref{c28}).

\begin{latin}
\begin{Rcode}
> library(gganimate)
> anim = p + 
+   transition_states(Species,
+                     transition_length = 2,
+                     state_length = 1)
> anim
\end{Rcode}
\end{latin}
\captionof{lstlisting}{کد مربوط به اضافه نمودن توابع بسته {gganimate} جهت ایجاد نمودار متحرک در محیط {R}.}
\label{c28}

\begin{center}
\includegraphics[scale = .9]{petal}
\end{center}
\captionof{figure}{در این تصویر یک نمودار متحرک را مشاهده می‌کنید، که با استفاده از تابع {transition-states()} در بسته \lr{gganimate} ایجاد شده است. تابع {transition-states()} داده‌های نمودار را به یک متغیر گستته تقسیم می‌کند و بین حالت‌های مختلف متحرک می‌کند.}
\label{n10}

همانطور که مشاهده می‌کنید، اضافه نمودن یک تابع به برنامه \eqref{c27}، سبب تبدیل نمودار ایستا به یک متحرک شده است. برای این کار ما یک نوع از انتقال را اضافه نموده‌ایم. انتقال‌ها توابعی هستند که با تقسیم کردن داده‌ها به قاب‌های مختلف سبب تفسیر داده‌ها می‌شوند. تابع {transition-states())} به طور خاص داده‌ها را بر اساس متغیر (زیر گروه‌ها) به زیر مجموعه‌ها تقسیم می‌کند.

درک یک نمودار متحرک بدون هیچ‌گونه نشانه‌ای در نمودار می‌تواند بسیار دشوار باشد. بسته {gganimate} با ارائه مجموعه‌ای از متغیرها و پارامترها برای هر قاب به ما کمک می‌کند تا بتوانیم علائم و نشانه‌های مربوطه را به نمودار متحرک خود اضافه نماییم. به عنوان مثال در ادامه مثل قبل می‌توانیم برای قرار دادن متغیرهای قاب در عنوان نمودار از تابع {ggtitle()} استفاده کنیم، (برنامه \eqref{c29}).


\begin{latin}
\begin{Rcode}
> anim + 
+ ggtitle('Now showing {closest_state}',
+         subtitle = 'Frame {frame} of {nframes}')
\end{Rcode}
\end{latin}
\captionof{lstlisting}{کد مربوط به اضافه نمودن متغیرهای هر قاب در عنوان نمودار.}
\label{c29}

انتقال‌های مختلف متغیرهای قاب‌های مختلف را ارائه می‌دهد. توجه داشته باشید که پارامتر {closest-state} تنها برای تابع {transition-states()} معنا پیدا می‌کند، بنابراین فقط در صورت استفاده از تابع {transition-states()} این پارامتر قابل استفاده است.

\begin{center}
\includegraphics[scale = .9]{transi2}
\end{center}
\captionof{figure}{در این تصویر یک نمودار متحرک را مشاهده می‌کنید. که داده‌های مربوط به 150 نمونه گل زنبق با توجه به نوعشان به سه گروه تقسیم شده‌اند، و نام هر گونه از گل‌ها در خط اول عنوان نمایش داده می‌شود. و در خط دوم عنوان نیز تعداد قاب‌ها مشخص شده است.}
\label{n11}

در مثال‌های فوق نمودارهای متحرک دقیقا مانند  نمودارهای ساخته شده با استفاده از بسته {ggplot2} به تصویر کشیده شده‌اند. بسیاری از مراحل چاپ نمودارهای متحرک به صورت خودکار اتفاق می‌افتد، اما گاهی اوقات ما می‌توانیم موارد پیش‌فرض را به دلخواه خود تغییر دهیم. که قصد داریم در ادامه بیشتر این موضوع را تشریح کنیم.

وقتی که یک شی متحرک را چاپ می‌کنیم، تابع {animate()} با آرگومان‌های پیش‌فرض بر روی تصویر متحرک اعمال می‌شود، برخی از این آرگومانها به شرح زیر هستند:

\begin{itemize}
\item
\textbf{nframes}:
تعداد قاب‌ (پیش‌فرض 100 قاب است) را مشخص می‌کند.
\item
\textbf{fps}:
تعداد قاب (پیش‌فرض 10 قاب است) را مشخص می‌کند.
\item
\textbf{dev}:
فرمت مورد نظر جهت ارائه هر قاب را تنظیم می‌کند (پیش‌فرض {"png"} است).
\item
\textbf{renderer}:
توابعی را برای ترکیب قاب‌ها در تابع {animate()} ارائه می‌دهد (پیش‌فرض تابع {gifski-renderer()} است).
\end{itemize}

آرگومان‌های دیگری نیز وجود دارد ( مثلا می‌توانیم عرض، ارتفاع، {dpi} و غیره را تنظیم کنیم)، اما آرگومان‌های ذکر شده در بالا آرگومان‌های مهم‌تری هستند. همانطور که قبلا اشاره کردیم به طور پیش‌فرض از {gifski} برای ترکیب قاب‌ها به یک {gif} استفاده می‌کنیم. {gif}ها عالی هستند زیرا تقریبا در همه جا پشتیبانی می‌شوند و {gifski} هم یک مبدل بسیار قوی و با کیفیت بسیار بالایی است. اما با این وجود ممکن است خواهان این باشیم که یک خروجی متفاوت داشته باشیم. خروجی ما در نهایت توسط تابع {animate()} مشخص می‌گردد. در زیر چند نمونه از استفاده‌های تابع {animate()} را مشاهده می‌کنید:

در ادامه مثال قبل قصد داریم نمودار متحرک خود را به صورت یک فیلم ارائه دهیم، پس مانند کد موجود در برنامه \eqref{c30} عمل می‌کنیم.

توجه داشته باشید که جهت انجام این کار ابتدا بایستی بسته {av} را فراخوانی کنیم.


\begin{latin}
\begin{Rcode}
> library(av)
> library(gganimate)
> animate(
+   anim ,
+   renderer = av_renderer()
+ )
\end{Rcode}
\end{latin}
\captionof{lstlisting}{استفاده از آرگومان {renderer} جهت ارائه خروجی خود به صورت یک فیلم.}
\label{c30}

\begin{center}
\includegraphics[scale = .9]{av1}
\end{center}
\captionof{figure}{در این تصویر یک نمودار متحرک را مشاهده می‌کنید. که به صورت یک فیلم نمایش داده شده است، این کار توسط آرگومان \lr{renderer} در بسته {gganimate} انجام گرفته است.}
\label{n12}


چنانچه بخواهیم نمودار متحرک خود را جهت استفاده‌های بعدی ذخیره کنیم، می‌توانیم از تابع {anim-save()} استفاده کنیم. این تابع بسیار شبیه به تابع {ggsave()} در بسته {ggplot2} عمل می‌کند. اگر در تابع مشخص نکنیم که قصد ذخیره کدام نمودار را داریم، این تابع به صورت پیش‌فرض آخرین نمودار را ذخیره می‌کند. نحوه استفاده از تابع {anim-save()} در برنامه \eqref{c31} نشان داده شده است.

\begin{latin}
\begin{Rcode} 
> anim_save(filename, plot name)
\end{Rcode}
\end{latin}
\captionof{lstlisting}{نحوه استفاده از تابع \lr{anim-save()} جهت ذخیره شی متحرک.}
\label{c31}

همانطور که در برنامه \eqref{c31} مشاهده می‌کنید، ابتدا نام پرونده‌ای که می‌خواهیم شی متحرک خود را در آن ذخیره کنیم، ذکر می‌کنیم، سپس نام تصویری را که می‌خواهیم ذخیره کنیم می‌نویسیم.
</div>
