/*
  CGILIB - Por Wagner Nunes da Silva - cgilib.ch : CliPHP.
  Lib CGI para uso com xHarbour

  http://www.vagucs.com.br
  E-mail   : vagucs@vagucs.com.br
             vagucs@bol.com.br
             vagucs@systemnet.com.br

  Rua Dom Bosco, 23
  Centro
  Ipanema - MG
  Cep: 36.950-000
  Tel: +55 (33) 3314-2242 - +55 (33) 9965-1578 - +55 (33) 3314-1578

  Todos os direitos reservados.
*/
#DEFINE BLACK      "#000000"
#DEFINE BLUE       "#000099" 
#DEFINE GREEN      "#339933" 
#DEFINE CYAN       "#009999" 
#DEFINE RED        "#FF0000" 
#DEFINE MAGENTA    "#990099" 
#DEFINE BROWN      "#996600" 
#DEFINE WHITE      "#999999" 
#DEFINE WBLACK     "#666666" 
#DEFINE WBLUE      "#6666FF" 
#DEFINE WGREEN     "#33FF66" 
#DEFINE WCYAN      "#66FFFF" 
#DEFINE WRED       "#FF6666" 
#DEFINE WMAGENTA   "#FF33FF" 
#DEFINE WBROWN     "#FFFF33" 
#DEFINE WWHITE     "#FFFFFF" 

#xcommand SET LF TO <TF> => if_lf:=<TF>

#xcommand ? <texto>     => cgi_qout(<texto>)
#xcommand ?? <texto>    => cgi_qqout(<texto>)

#xcommand OUT <texto> => outstd(<texto>+chr(13))

#xcommand ENABLE CONVERSION OEMTOANSI   => set_conversion(1)
#xcommand ENABLE CONVERSION ANSITOOEM   => set_conversion(2)
#xcommand DISABLE CONVERSION => set_conversion(0)

// Para compatibilidade com a versÆo 1.0

#xcommand TEXT => text xcgi_qout,xcgi_qqout

#xcommand SET QUOTE TO <quotestr> => cgi_quote_str:=<quotestr>

#xtranslate LINE [WIDTH <W>] [HEIGHT <H>][ID <ID>] => html_line(<W>,<H>,<ID>)

#xcommand HEAD => outstd("<head>"+chr(13)+chr(10))
#xtranslate END HEAD => outstd("</head>"+chr(13)+chr(10))

#xcommand HTML => outstd("<html>"+chr(13)+chr(10))
#xtranslate END HTML => outstd("</html>"+chr(13)+chr(10))


#xtranslate ALLIGN CENTER                     => outstd("<div align=center>"+chr(13)+chr(10))
#xtranslate ALLIGN LEFT                       => outstd("<div align=left>"+chr(13)+chr(10))
#xtranslate ALLIGN RIGHT                      => outstd("<div align=right>"+chr(13)+chr(10))
#xtranslate ALLIGN NONE                       => outstd("<div align=none>"+chr(13)+chr(10))
#xtranslate END ALLIGN                        => outstd("</div>"+chr(13)+chr(10))

#xcommand SET TITLE <TITULO> => html_title(<TITULO>)

#xtranslate BODY [<COR1>] [FONT <COR2>]=> html_body(<COR1>,<COR2>)
#xtranslate END BODY => outstd("</body>"+chr(13)+chr(10))

#xtranslate SET FONT [TO <FONT>] [SIZE <TAMANHO>];
            [COLOR <COR>] => html_font(<FONT>,<TAMANHO>,<COR>)
#xcommand END FONT                              => outstd("</font>"+chr(13)+chr(10))

// Alinhamentos: baseline,top,middle,bottom,texttop,absmiddle,absbottom
//               left,right,none

#xtranslate IMAGE <FILE> [HEIGHT <H>] [WIDTH <W>] ;
            [ALIGN <ALG>] [VSPACE <VS>] [HSPACE <HS>] ;
            [BORDER <BD>] => html_image(<FILE>,<H>,<W>,<ALG>,<VS>,<HS>,<BD>)

#xtranslate BEGIN TABLE <OTAB> [BORDER <BD>] [WIDTH <W>] ;
            [HEIGHT <H>] [CELLSPACING <CS>] [ALIGN <AL>] ;
            [BGCOLOR <BGCOLOR>] [BORDERCOLOR <BDCOLOR>] ;
            [BACKGROUND <IMG>] ;
            [CELLPADDING <CLP>] => html_table(<OTAB>,<BD>,<W>,<H>,<CS>,<AL>,<BGCOLOR>,<BDCOLOR>,<IMG>,<CLP>)
#xcommand END TABLE => outstd("</table>"+chr(13)+chr(10))

#xtranslate NEW ROW => outstd("<tr>"+chr(13)+chr(10))
#xtranslate END ROW => outstd("</tr>"+chr(13)+chr(10))

#xtranslate NEW COL [BGCOLOR <BGC>];
            [BORDER COLOR <BDC>] [WIDTH <W>] [HEIGHT <H>] ;
            [ALLING <AL>] [VALLIGN <VL>];
            [<NW>] => html_col(<BGC>,<BDC>,<W>,<H>,<AL>,<VL>,<NW>)
#xtranslate END COL => outstd("</td>"+chr(13)+chr(10))

#xtranslate NEW COL HEADER [BGCOLOR <BGC>];
            [BORDER COLOR <BDC>] [WIDTH <W>] [HEIGHT <H>] ;
            [ALLING <AL>] [VALLIGN <VL>] ;
            [<NW>] => html_col_header(<BGC>,<BDC>,<W>,<H>,<AL>,<VL>,<NW>)
#xtranslate END COLHEADER => outstd("</th>"+chr(13)+chr(10))

#xtranslate BOLD           => outstd("<b>"+chr(13)+chr(10))
#xtranslate END BOLD       => outstd("</b>"+chr(13)+chr(10))

#xtranslate ITALIC         => outstd("<i>"+chr(13)+chr(10))
#xtranslate END ITALIC     => outstd("</i>"+chr(13)+chr(10))

#xtranslate UNDERLINE      => outstd("<u>"+chr(13)+chr(10))
#xtranslate END UNDERLINE  => outstd("</u>"+chr(13)+chr(10))

#xtranslate LIST             => outstd("<ul>"+chr(13)+chr(10))
#xtranslate END LIST         => outstd("</ul>"+chr(13)+chr(10))

#xtranslate ORDERED LIST     => outstd("<ol>"+chr(13)+chr(10))
#xtranslate END ORDERED LIST => outstd("</ol>"+chr(13)+chr(10))

#xtranslate ELEMENT          => outstd("<li>"+chr(13)+chr(10))
#xtranslate END ELEMENT      => outstd("</li>"+chr(13)+chr(10))

#xtranslate QUOTE            => outstd("<blockquote>"+chr(13)+chr(10))
#xtranslate END QUOTE        => outstd("</blockquote>"+chr(13)+chr(10))

#xtranslate LINK <LINK> [TARGET <TARG>] => html_link(<LINK>,<TARG>)
#xtranslate END LINK         => outstd("</a>"+chr(13)+chr(10))

#xtranslate MARQUEE          => outstd("<marquee>"+chr(13)+chr(10))
#xtranslate END MARQUEE      => outstd("</marquee>"+chr(13)+chr(10))

#xtranslate PREFORMATED      => outstd("<pre>"+chr(13)+chr(10))
#xtranslate END PREFORMATED  => outstd("</pre>"+chr(13)+chr(10))

#xtranslate HEADING 1        => outstd("<h1>"+chr(13)+chr(10))
#xtranslate END HEADING 1    => outstd("</h1>"+chr(13)+chr(10))

#xtranslate HEADING 2        => outstd("<h2>"+chr(13)+chr(10))
#xtranslate END HEADING 2    => outstd("</h2>"+chr(13)+chr(10))

#xtranslate HEADING 3        => outstd("<h3>"+chr(13)+chr(10))
#xtranslate END HEADING 3    => outstd("</h3>"+chr(13)+chr(10))

#xtranslate HEADING 4        => outstd("<h4>"+chr(13)+chr(10))
#xtranslate END HEADING 4    => outstd("</h4>"+chr(13)+chr(10))

#xtranslate HEADING 5        => outstd("<h5>"+chr(13)+chr(10))
#xtranslate END HEADING 5    => outstd("</h5>"+chr(13)+chr(10))

#xtranslate BIG              => outstd("<big>"+chr(13)+chr(10))
#xtranslate END BIG          => outstd("</big>"+chr(13)+chr(10))

#xtranslate SMALL            => outstd("<small>"+chr(13)+chr(10))
#xtranslate END SMALL        => outstd("</small>"+chr(13)+chr(10))

#xtranslate SUBSCRIPT        => outstd("<sub>"+chr(13)+chr(10))
#xtranslate END SUBSCRIPT    => outstd("</sub>"+chr(13)+chr(10))

#xtranslate SUPSCRIPT        => outstd("<sup>"+chr(13)+chr(10))
#xtranslate END SUPSCRIPT    => outstd("</sup>"+chr(13)+chr(10))

#xtranslate PARAGRAPH        => outstd("<p>"+chr(13)+chr(10))
#xtranslate END PARAGRAPH    => outstd("</p>"+chr(13)+chr(10))

* DEFINI€åES DE FORMULµRIOS

#xtranslate TEXTBOX NAME <NOME> [TEXT <TEXT>] [SIZE <SIZE>] [VALUE <VALUE>];
                    [MAXLENGTH <MAX>] => form_input(<NOME>,"text",<VALUE>,<SIZE>,<MAX>)

#xtranslate PASSWORD NAME <NOME> [TEXT <TEXT>] [SIZE <SIZE>] [VALUE <VALUE>];
                     [MAXLENGTH <MAX>] => form_input(<NOME>,"password",<VALUE>,<SIZE>,<MAX>)

* WRAP = OFF / VIRTUAL / PHYSICAL

#xtranslate  TEXTAREA NAME <NOME> [COLS <COLS>] [ROWS <ROWS>] [WRAP <WRAP>];
                      [VALUE <VALOR>] => form_textarea(<NOME>,<COLS>,<ROWS>,<WRAP>,<VALOR>)

#xtranslate SELECT NAME <NOME> [SIZE <SIZE>] [MULTIPLE <MTL>] ;
                   [OPTIONS <ARRAY>] [SELECTED <ITEM>] => form_select(<NOME>,<SIZE>,<MTL>,<ARRAY>,<ITEM>)

#xtranslate BUTTON NAME <NOME> [TYPE <TIPO>] [VALUE <VALOR>] [ACTION <ACTION>];
                        [ONCLICK <JAVAFUNC>];
                        [METHOD <METHOD>] => form_button(<NOME>,<TIPO>,<VALOR>,<ACTION>,<METHOD>,<JAVAFUNC>)

#xtranslate BEGIN FORM NAME <NOME> [ACTION <ACTION>] [METHOD <METHOD>];
                 [ENCODER <ENC>] ;
                 [TARGET <TARGET>] => form_begin(<NOME>,<ACTION>,<METHOD>,<ENC>,<TARGET>)

#xtranslate END FORM => outstd("</form>"+chr(13)+chr(10))
