$PBExportHeader$w_cons_proceso_en_linea.srw
$PBExportComments$Consulta de Procesos en Línea
forward
global type w_cons_proceso_en_linea from window
end type
type uo_selplanta from uo_seleccion_plantas within w_cons_proceso_en_linea
end type
type pb_imprimir from picturebutton within w_cons_proceso_en_linea
end type
type dw_2 from datawindow within w_cons_proceso_en_linea
end type
type dw_1 from datawindow within w_cons_proceso_en_linea
end type
type sle_prod from singlelineedit within w_cons_proceso_en_linea
end type
type sle_codpro from singlelineedit within w_cons_proceso_en_linea
end type
type st_20 from statictext within w_cons_proceso_en_linea
end type
type sle_varie from singlelineedit within w_cons_proceso_en_linea
end type
type sle_codvar from singlelineedit within w_cons_proceso_en_linea
end type
type sle_espe from singlelineedit within w_cons_proceso_en_linea
end type
type sle_codesp from singlelineedit within w_cons_proceso_en_linea
end type
type cb_1 from commandbutton within w_cons_proceso_en_linea
end type
type em_fecha from editmask within w_cons_proceso_en_linea
end type
type st_8 from statictext within w_cons_proceso_en_linea
end type
type em_proceso from editmask within w_cons_proceso_en_linea
end type
type st_7 from statictext within w_cons_proceso_en_linea
end type
type ddlb_tipoproc from dropdownlistbox within w_cons_proceso_en_linea
end type
type st_6 from statictext within w_cons_proceso_en_linea
end type
type pb_salir from picturebutton within w_cons_proceso_en_linea
end type
type pb_acepta from picturebutton within w_cons_proceso_en_linea
end type
type st_4 from statictext within w_cons_proceso_en_linea
end type
type st_2 from statictext within w_cons_proceso_en_linea
end type
type st_1 from statictext within w_cons_proceso_en_linea
end type
type st_titulo from statictext within w_cons_proceso_en_linea
end type
type st_encab from statictext within w_cons_proceso_en_linea
end type
end forward

global type w_cons_proceso_en_linea from window
string tag = "Genera Consultas en Linea para Selección de Procesos"
integer x = 1074
integer y = 484
integer width = 4005
integer height = 1956
boolean titlebar = true
string title = "Consulta de Procesos en Línea"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 16777215
uo_selplanta uo_selplanta
pb_imprimir pb_imprimir
dw_2 dw_2
dw_1 dw_1
sle_prod sle_prod
sle_codpro sle_codpro
st_20 st_20
sle_varie sle_varie
sle_codvar sle_codvar
sle_espe sle_espe
sle_codesp sle_codesp
cb_1 cb_1
em_fecha em_fecha
st_8 st_8
em_proceso em_proceso
st_7 st_7
ddlb_tipoproc ddlb_tipoproc
st_6 st_6
pb_salir pb_salir
pb_acepta pb_acepta
st_4 st_4
st_2 st_2
st_1 st_1
st_titulo st_titulo
st_encab st_encab
end type
global w_cons_proceso_en_linea w_cons_proceso_en_linea

type variables
str_busqueda	istr_busq
Str_info		lstr_info

DataWindowChild	 idwc_planta

uo_plantadesp      iuo_planta
uo_especie			iuo_Especie

Integer    ii_tiponum
Long		  ll_BultosInject=0
dec{2}     ld_kilosInject=0

Transaction itr_inject


end variables

forward prototypes
public subroutine habilitagrabar ()
public subroutine conectainject ()
public function double Buscapesoembalaje (integer ai_fila)
public subroutine calculadatosinject ()
public function boolean buscaordenreproceso (long al_orden)
public function boolean buscaorden (long al_orden, integer ai_tipo)
public function boolean buscadoctointerno (integer ai_tipo, long al_numero)
end prototypes

public subroutine habilitagrabar ();
end subroutine

public subroutine conectainject ();SetPointer(HourGlass!)

String	ls_nombre, ls_Dbms
Integer	li_posic, li_resp

itr_inject.SQLCode	=	1

ls_nombre		=	ProfileString(gstr_apl.ini, gs_base, "Inject", "")
								
itr_inject.DBMS = "ODBC"
itr_inject.AutoCommit = False
itr_inject.DBParm = "ConnectString='DSN="+ls_nombre+";UID=;PWD='"

SetPointer(HourGlass!)

CONNECT Using itr_inject; 

IF itr_inject.SQLCode <> 0 THEN
	IF itr_inject.SQLDBCode = -103 THEN
		IF MessageBox("Error de Conexión", "Usuario o Password ingresado está incorrecto.", &
								Information!, RetryCancel!) = 1 THEN
			END IF
			
			RETURN
	ELSEIF itr_inject.SQLDBCode = -102 THEN
			MessageBox("Error de Conexión", "Demasiados Usuarios conectados a la Base.~r" + &
							"Consulte con Administrador", StopSign!, Ok!)
				RETURN
	ELSEIF itr_inject.SQLDBCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		RETURN
	END IF
END IF

//Llamar a datawindow
dw_2.SetTransObject(itr_inject)
dw_2.Retrieve(Date(em_fecha.text))

SetPointer(Arrow!)
end subroutine

public function double Buscapesoembalaje (integer ai_fila);Integer li_variedad, li_especie
String  ls_embalaje, ls_calibre
Dec{2}  ld_kilos

li_variedad = dw_2.Object.variedad[ai_fila]
li_especie  = integer(mid(string(li_variedad,'0000'),1,2))
ls_embalaje = dw_2.Object.envase[ai_fila]
ls_calibre  = dw_2.Object.calibre[ai_fila]

SELECT pemb_kilcua INTO :ld_kilos
  FROM dbo.pesosembalaje
 WHERE espe_codigo = :li_especie
   AND vari_codigo = :li_variedad
	AND emba_codigo = :ls_embalaje
	AND pemb_calibr = :ls_calibre;
	
	
IF ld_Kilos=0 or isnull(ld_kilos) THEN
   SetNull(li_variedad)
	SELECT pemb_kilcua INTO :ld_kilos
     FROM dbo.pesosembalaje
    WHERE espe_codigo = :li_especie
      AND vari_codigo = :li_variedad
	   AND emba_codigo = :ls_embalaje
	   AND pemb_calibr = :ls_calibre;
	
END IF

IF isnull(ld_Kilos) THEN ld_Kilos=0


RETURN ld_Kilos
end function

public subroutine calculadatosinject ();Long ll_bultos,ll_fila
Dec {2} ld_kilos
ld_kilosInject=0

ll_bultos = 0
FOR ll_fila=1 To dw_2.RowCount()
	IF dw_2.Object.variedad[ll_fila] = integer(sle_codvar.text) THEN
		ll_bultos = dw_2.Object.caja[ll_fila]
		IF isnull(ll_bultos) THEN ll_Bultos=0
		ll_BultosInject = ll_BultosInject + ll_Bultos
		ld_kilos = Buscapesoembalaje(ll_fila)
		ld_kilosInject = ld_kilosInject   + (ll_Bultos*ld_kilos)
	END IF
	IF ii_tiponum > 4 THEN
		ll_bultos = dw_2.Object.caja[ll_fila]
		IF isnull(ll_bultos) THEN ll_Bultos=0
		ll_BultosInject = ll_BultosInject + ll_Bultos
		ld_kilos = Buscapesoembalaje(ll_fila)
		ld_kilosInject = ld_kilosInject   + (ll_Bultos*ld_kilos)
	END IF	
NEXT	
	
end subroutine

public function boolean buscaordenreproceso (long al_orden);Integer	li_Especie, li_Estado
Date  		ldt_Fecha
Boolean	lb_Retorno = False


SELECT	espe_codigo, dinp_estado, dinp_fecdoc
	INTO	:li_Especie, :li_Estado, :ldt_Fecha
	FROM	dbo.spro_doctointernopack
	WHERE	plde_codigo	=	:uo_SelPlanta.Codigo
	AND	dinp_tipdoc	=	5
	AND	dinp_numero	=	:al_Orden;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_doctointernopack")
	Return False
ELSEIF sqlca.SQLCode <> 100 THEN
	iuo_Especie.Existe(li_Especie, False, sqlca)

	sle_codesp.text	=	String(li_Especie, '00')
	sle_espe.text		=	iuo_Especie.Nombre
	em_fecha.text		=	String(ldt_Fecha)
	
END IF

RETURN lb_Retorno
end function

public function boolean buscaorden (long al_orden, integer ai_tipo);Integer li_especie, li_variedad, li_estado
String  ls_productor, ls_especie, ls_variedad
Date    ld_fecha
Long    ll_productor

IF isnull(ai_tipo) or ai_tipo=0 THEN
	messagebox("Atención","Debe Seleccionar un Tipo de Proceso.")
	RETURN FALSE
END IF

SELECT prod_codigo, espe_codigo, vari_codigo, orpr_fecpro, orpr_estado
  INTO :ll_productor, :li_especie, :li_variedad, :ld_fecha, :li_estado
  FROM dbo.spro_ordenproceso
 WHERE plde_codigo	=	:uo_SelPlanta.Codigo
   and orpr_tipord	=	:ai_tipo
	and orpr_numero	=	:al_orden;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Spro_Ordenproceso")
	RETURN FALSE
ELSEIF sqlca.SQLCode <> 100 THEN

   SELECT prod_nombre
  		INTO :ls_productor
  		FROM dbo.productores
	 WHERE prod_codigo	=	:ll_productor;

	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Productores")
	END IF
	
	SELECT espe_nombre
  		INTO :ls_especie
  		FROM dbo.especies
	 WHERE espe_codigo	=	:li_especie;

	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Especies")
	END IF

	SELECT vari_nombre
  		INTO :ls_variedad
  		FROM dbo.variedades
	 WHERE espe_codigo	=	:li_especie
	   and vari_codigo	=	:li_variedad;

	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Variedades")
	END IF

   //asigna datos
	
	em_fecha.text		=  string(ld_fecha,'dd/mm/yyyy')

	sle_codpro.text	=	String(ll_productor,'00000')
	sle_prod.text		=	ls_productor
	
	sle_codesp.text	=	String(li_especie,'00')
	sle_espe.text		=	ls_especie
	
	sle_codvar.text	=	String(li_variedad,'0000')
	sle_varie.text		=	ls_variedad
	
	RETURN TRUE
END IF

RETURN FALSE
end function

public function boolean buscadoctointerno (integer ai_tipo, long al_numero);Integer li_especie
String  ls_especie
Date    ld_fecha

IF isnull(ai_tipo) or ai_tipo=0 THEN
	messagebox("Atención","Debe Seleccionar un Tipo de Proceso.")
	RETURN FALSE
END IF		

SELECT espe_codigo, dinp_fecdoc
  INTO :li_especie, :ld_fecha
  FROM dbo.spro_doctointernopack
 WHERE plde_codigo	=	:uo_SelPlanta.Codigo
   and dinp_tipdoc	=	:ai_tipo
	and dinp_numero	=	:al_numero;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla documento interno packing")
	RETURN FALSE
ELSEIF sqlca.SQLCode <> 100 THEN

	SELECT espe_nombre
  		INTO :ls_especie
  		FROM dbo.especies
	 WHERE espe_codigo	=	:li_especie;

	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Especies")
	END IF

   //asigna datos
	
	em_fecha.text		=  string(ld_fecha,'dd/mm/yyyy')

	sle_codesp.text	=	String(li_especie,'00')
	sle_espe.text		=	ls_especie
	
	RETURN TRUE
	
END IF

RETURN FALSE
end function

on w_cons_proceso_en_linea.create
this.uo_selplanta=create uo_selplanta
this.pb_imprimir=create pb_imprimir
this.dw_2=create dw_2
this.dw_1=create dw_1
this.sle_prod=create sle_prod
this.sle_codpro=create sle_codpro
this.st_20=create st_20
this.sle_varie=create sle_varie
this.sle_codvar=create sle_codvar
this.sle_espe=create sle_espe
this.sle_codesp=create sle_codesp
this.cb_1=create cb_1
this.em_fecha=create em_fecha
this.st_8=create st_8
this.em_proceso=create em_proceso
this.st_7=create st_7
this.ddlb_tipoproc=create ddlb_tipoproc
this.st_6=create st_6
this.pb_salir=create pb_salir
this.pb_acepta=create pb_acepta
this.st_4=create st_4
this.st_2=create st_2
this.st_1=create st_1
this.st_titulo=create st_titulo
this.st_encab=create st_encab
this.Control[]={this.uo_selplanta,&
this.pb_imprimir,&
this.dw_2,&
this.dw_1,&
this.sle_prod,&
this.sle_codpro,&
this.st_20,&
this.sle_varie,&
this.sle_codvar,&
this.sle_espe,&
this.sle_codesp,&
this.cb_1,&
this.em_fecha,&
this.st_8,&
this.em_proceso,&
this.st_7,&
this.ddlb_tipoproc,&
this.st_6,&
this.pb_salir,&
this.pb_acepta,&
this.st_4,&
this.st_2,&
this.st_1,&
this.st_titulo,&
this.st_encab}
end on

on w_cons_proceso_en_linea.destroy
destroy(this.uo_selplanta)
destroy(this.pb_imprimir)
destroy(this.dw_2)
destroy(this.dw_1)
destroy(this.sle_prod)
destroy(this.sle_codpro)
destroy(this.st_20)
destroy(this.sle_varie)
destroy(this.sle_codvar)
destroy(this.sle_espe)
destroy(this.sle_codesp)
destroy(this.cb_1)
destroy(this.em_fecha)
destroy(this.st_8)
destroy(this.em_proceso)
destroy(this.st_7)
destroy(this.ddlb_tipoproc)
destroy(this.st_6)
destroy(this.pb_salir)
destroy(this.pb_acepta)
destroy(this.st_4)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.st_titulo)
destroy(this.st_encab)
end on

event open;Boolean	lb_Cerrar = False

If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	X	=	0
	Y	=	0
	This.Height	= 2500
	
	uo_SelPlanta.Seleccion(False, False)
	uo_SelPlanta.Inicia(gstr_ParamPlanta.CodigoPlanta)
	
	em_fecha.text = string(today(),'dd/mm/yyy')
	
	ddlb_tipoproc.text = '1. - Proceso'
	ii_tiponum = 4
	
	iuo_Especie		=	CREATE uo_Especie
	
	itr_inject = CREATE Transaction
End If
end event

event resize;Integer	maximo, li_posic_x, li_posic_y, li_visible = 0, &
			li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

dw_1.width	=	This.WorkSpaceWidth() - 600
maximo		=	dw_1.width


dw_1.x					=	37 + Round((maximo - dw_1.width) / 2, 0)
dw_1.y					=	64 + st_encab.y + st_encab.Height
dw_1.height				=	This.WorkSpaceHeight() - dw_1.y - 41

li_posic_x				=	This.WorkSpaceWidth() - 370
li_posic_y				=	dw_1.y

IF pb_Acepta.Visible THEN
	pb_Acepta.x				=	li_posic_x
	pb_Acepta.y				=	li_posic_y
	pb_Acepta.width		=	li_Ancho
	pb_Acepta.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			=	li_posic_x
	pb_imprimir.y			=	li_posic_y
	pb_imprimir.width		=	li_Ancho
	pb_imprimir.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

IF pb_salir.Visible THEN
	pb_salir.x				=	li_posic_x
	pb_salir.y				=	li_posic_y
	pb_salir.width			=	li_Ancho
	pb_salir.height		=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF
end event

type uo_selplanta from uo_seleccion_plantas within w_cons_proceso_en_linea
event destroy ( )
integer x = 590
integer y = 228
integer height = 96
integer taborder = 40
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type pb_imprimir from picturebutton within w_cons_proceso_en_linea
integer x = 3378
integer y = 924
integer width = 302
integer height = 244
integer taborder = 70
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Imprimir.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Imprimir-bn.png"
alignment htextalign = right!
end type

event clicked;SetPointer(Arrow!)
 
str_info istr_info

Long ll_fila

IF dw_1.RowCount()<=0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
	RETURN
END IF 

istr_info.titulo	= 'CONSULTA DE PROCESOS EN LINEA'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_consulta_proceso_linea"
vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.InsertRow(0)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.setitem(1,"plde_codigo",uo_SelPlanta.Codigo)
	vinf.dw_1.setitem(1,"tipo_proces",ii_tiponum)
	vinf.dw_1.setitem(1,"orpr_numero",integer(em_proceso.text))
	vinf.dw_1.setitem(1,"orpr_fecpro",date(em_fecha.text))
	vinf.dw_1.setitem(1,"prod_codigo",long(sle_codpro.text))
	vinf.dw_1.setitem(1,"prod_nombre",sle_prod.text)
	vinf.dw_1.setitem(1,"espe_codigo",integer(sle_codesp.text))
	vinf.dw_1.setitem(1,"espe_nombre",sle_espe.text)
	vinf.dw_1.setitem(1,"vari_codigo",integer(sle_codvar.text))
	vinf.dw_1.setitem(1,"vari_nombre",sle_varie.text)
   vinf.dw_1.setitem(1,"vaci_traspa_fg",dw_1.Object.vac_trasp_fg[1])
	vinf.dw_1.setitem(1,"vaci_confir_fg",dw_1.Object.vac_conf_fg[1])
	vinf.dw_1.setitem(1,"cant_cajas_em",dw_1.Object.cant_caja_em[1])
	vinf.dw_1.setitem(1,"cant_kilos_em",dw_1.Object.cant_kilo_em[1])
	vinf.dw_1.setitem(1,"cant_cajas_in",dw_1.Object.cant_bult_in[1])
	vinf.dw_1.setitem(1,"cant_kilos_in",dw_1.Object.cant_kilo_in[1])
	vinf.dw_1.setitem(1,"vac_trasp_co",dw_1.Object.vac_trasp_co[1])
	vinf.dw_1.setitem(1,"vac_confir_co",dw_1.Object.vac_conf_co[1])
	
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type dw_2 from datawindow within w_cons_proceso_en_linea
string tag = "Conecta a Access para Datos de Inject"
boolean visible = false
integer x = 197
integer y = 1144
integer width = 2766
integer height = 492
integer taborder = 70
string title = "none"
string dataobject = "dw_mues_datos_inject"
boolean hscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_1 from datawindow within w_cons_proceso_en_linea
integer x = 82
integer y = 740
integer width = 2994
integer height = 1036
integer taborder = 70
string title = "none"
string dataobject = "dw_mues_consulta_en_linea"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type sle_prod from singlelineedit within w_cons_proceso_en_linea
integer x = 2075
integer y = 460
integer width = 905
integer height = 104
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_codpro from singlelineedit within w_cons_proceso_en_linea
integer x = 1883
integer y = 460
integer width = 174
integer height = 104
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_20 from statictext within w_cons_proceso_en_linea
integer x = 1495
integer y = 460
integer width = 370
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type sle_varie from singlelineedit within w_cons_proceso_en_linea
integer x = 2071
integer y = 584
integer width = 905
integer height = 104
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_codvar from singlelineedit within w_cons_proceso_en_linea
integer x = 1883
integer y = 584
integer width = 174
integer height = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_espe from singlelineedit within w_cons_proceso_en_linea
integer x = 777
integer y = 584
integer width = 686
integer height = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_codesp from singlelineedit within w_cons_proceso_en_linea
integer x = 590
integer y = 584
integer width = 174
integer height = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type cb_1 from commandbutton within w_cons_proceso_en_linea
integer x = 2309
integer y = 344
integer width = 110
integer height = 88
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;Str_busqueda	lstr_busq

CHOOSE CASE ii_tiponum
	CASE 4
		lstr_busq.argum[1] = string(uo_SelPlanta.Codigo)
		lstr_busq.argum[2] = "1"
		lstr_busq.argum[3] = string(ii_tiponum)
		lstr_busq.argum[4] = '-1'
		lstr_busq.argum[6] = ""
		
		OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)
		
		lstr_busq	= Message.PowerObjectParm
		
		IF lstr_busq.argum[6] <> "" THEN
			dw_1.Reset()
			em_proceso.text = lstr_busq.argum[6]
			IF NOT BuscaOrden(long(lstr_busq.argum[6]),ii_tiponum) THEN
				 em_proceso.text = ""
			END IF	
		END IF

	CASE 5
		lstr_busq.argum[1] = "5"	
		OpenWithParm(w_busqueda_doctointernopack, lstr_busq)
		lstr_busq	= Message.PowerObjectParm
		
		IF lstr_busq.argum[3] <> "" THEN
			dw_1.Reset()
			em_proceso.text = lstr_busq.argum[3]
			IF NOT Buscadoctointerno(ii_tiponum,long(lstr_busq.argum[3])) THEN
				 em_proceso.text = ""
			END IF	
		END IF
END CHOOSE

RETURN 1
end event

type em_fecha from editmask within w_cons_proceso_en_linea
integer x = 590
integer y = 460
integer width = 462
integer height = 96
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "none"
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_8 from statictext within w_cons_proceso_en_linea
integer x = 165
integer y = 460
integer width = 370
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_proceso from editmask within w_cons_proceso_en_linea
integer x = 1883
integer y = 340
integer width = 384
integer height = 96
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 1090519039
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long ll_orden

ll_orden = long(this.text)
IF ii_tiponum=4 THEN
	dw_1.Reset()
	IF NOT BuscaOrden(ll_orden,ii_tiponum) THEN
		this.text=""
		sle_codpro.text=	""
		sle_prod.text	=	""
		sle_codesp.text=	""
		sle_espe.text	=	""
		sle_codvar.text=	""
		sle_varie.text	=	""
	END IF
ELSE
	dw_1.Reset()
	IF NOT BuscaOrdenReProceso(ll_orden) THEN
		sle_codpro.text=	""
		sle_prod.text	=	""
		sle_codesp.text=	""
		sle_espe.text	=	""
		sle_codvar.text=	""
		sle_varie.text	=	""
	END IF
END IF	
end event

type st_7 from statictext within w_cons_proceso_en_linea
integer x = 1495
integer y = 340
integer width = 370
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nº Proceso"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type ddlb_tipoproc from dropdownlistbox within w_cons_proceso_en_linea
integer x = 590
integer y = 340
integer width = 640
integer height = 284
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 16777215
string text = "none"
string item[] = {"1. - Proceso","2. - Re- Proceso"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_tiponum = Index + 3

sle_codpro.text	=	""
sle_prod.text		=	""
sle_codesp.text	=	""
sle_espe.text		=	""
sle_codvar.text	=	""
sle_varie.text		=	""
em_proceso.text	=	""
end event

type st_6 from statictext within w_cons_proceso_en_linea
integer x = 165
integer y = 228
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_cons_proceso_en_linea
integer x = 3387
integer y = 1264
integer width = 302
integer height = 244
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = right!
end type

event clicked;disconnect using itr_inject;
Close(Parent)
end event

type pb_acepta from picturebutton within w_cons_proceso_en_linea
event clicked pbm_bnclicked
integer x = 3378
integer y = 600
integer width = 302
integer height = 244
integer taborder = 50
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos-bn.png"
alignment htextalign = right!
end type

event clicked;Integer		li_variedad, li_respuesta, li_especie
Long			fila, ll_numero
Date 			ld_fecha, ld_fechapru

SetPointer(HourGlass!)

IF IsNull(ii_tiponum) THEN
	MessageBox("Atención","Debe Seleccionar Un Tipo de Proceso",Exclamation!)
	Return 1
END IF

ll_numero = long(em_proceso.text)
IF isnull(ll_numero) or ll_numero =0 THEN
	MessageBox("Atención","Debe Seleccionar un Numero de Proceso.",Exclamation!)
	Return 1
END IF	

dw_1.Reset()
dw_2.Reset()
ll_BultosInject = 0
ld_KilosInject  = 0
ConectaInject()

CalculaDatosInject()

dw_1.SetTransObject(sqlca)
dw_1.Retrieve(uo_SelPlanta.Codigo,ii_tiponum,ll_numero)

IF dw_1.RowCount() > 0 THEN
	dw_1.SetItem(1,"cant_bult_in", ll_BultosInject)
	dw_1.SetItem(1,"cant_Kilo_in", ld_kilosInject)
ELSE
	dw_1.InsertRow(0)
	dw_1.SetItem(1,"cant_bult_in", ll_BultosInject)
	dw_1.SetItem(1,"cant_Kilo_in", ld_kilosInject)
END IF	

SetPointer(Arrow!)

end event

type st_4 from statictext within w_cons_proceso_en_linea
integer x = 1495
integer y = 584
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Variedad"
boolean focusrectangle = false
end type

type st_2 from statictext within w_cons_proceso_en_linea
integer x = 165
integer y = 340
integer width = 393
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Tipo Proceso"
boolean focusrectangle = false
end type

type st_1 from statictext within w_cons_proceso_en_linea
integer x = 165
integer y = 584
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_titulo from statictext within w_cons_proceso_en_linea
integer x = 82
integer y = 68
integer width = 2990
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
string text = "Consulta de Proceso en Línea"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_encab from statictext within w_cons_proceso_en_linea
integer x = 82
integer y = 188
integer width = 3003
integer height = 536
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

