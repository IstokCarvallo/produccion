$PBExportHeader$w_anula_cierre_orden_proceso.srw
$PBExportComments$Genera Datos para Pronóstico de Cierre
forward
global type w_anula_cierre_orden_proceso from window
end type
type uo_SelCliente from uo_seleccion_clientesprod within w_anula_cierre_orden_proceso
end type
type uo_SelPlanta from uo_seleccion_plantas within w_anula_cierre_orden_proceso
end type
type st_12 from statictext within w_anula_cierre_orden_proceso
end type
type st_9 from statictext within w_anula_cierre_orden_proceso
end type
type sle_prod from singlelineedit within w_anula_cierre_orden_proceso
end type
type sle_codpro from singlelineedit within w_anula_cierre_orden_proceso
end type
type st_20 from statictext within w_anula_cierre_orden_proceso
end type
type sle_varie from singlelineedit within w_anula_cierre_orden_proceso
end type
type sle_codvar from singlelineedit within w_anula_cierre_orden_proceso
end type
type sle_espe from singlelineedit within w_anula_cierre_orden_proceso
end type
type sle_codesp from singlelineedit within w_anula_cierre_orden_proceso
end type
type cb_1 from commandbutton within w_anula_cierre_orden_proceso
end type
type em_fecha from editmask within w_anula_cierre_orden_proceso
end type
type st_8 from statictext within w_anula_cierre_orden_proceso
end type
type em_proceso from editmask within w_anula_cierre_orden_proceso
end type
type st_7 from statictext within w_anula_cierre_orden_proceso
end type
type ddlb_tipoproc from dropdownlistbox within w_anula_cierre_orden_proceso
end type
type st_6 from statictext within w_anula_cierre_orden_proceso
end type
type sle_mensa from singlelineedit within w_anula_cierre_orden_proceso
end type
type st_5 from statictext within w_anula_cierre_orden_proceso
end type
type pb_salir from picturebutton within w_anula_cierre_orden_proceso
end type
type pb_acepta from picturebutton within w_anula_cierre_orden_proceso
end type
type st_4 from statictext within w_anula_cierre_orden_proceso
end type
type st_2 from statictext within w_anula_cierre_orden_proceso
end type
type st_1 from statictext within w_anula_cierre_orden_proceso
end type
type st_titulo from statictext within w_anula_cierre_orden_proceso
end type
type st_3 from statictext within w_anula_cierre_orden_proceso
end type
type r_1 from rectangle within w_anula_cierre_orden_proceso
end type
end forward

global type w_anula_cierre_orden_proceso from window
integer x = 1074
integer y = 484
integer width = 2368
integer height = 1632
boolean titlebar = true
string title = "Anulación"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 16777215
event ue_validapassword ( )
uo_SelCliente uo_SelCliente
uo_SelPlanta uo_SelPlanta
st_12 st_12
st_9 st_9
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
sle_mensa sle_mensa
st_5 st_5
pb_salir pb_salir
pb_acepta pb_acepta
st_4 st_4
st_2 st_2
st_1 st_1
st_titulo st_titulo
st_3 st_3
r_1 r_1
end type
global w_anula_cierre_orden_proceso w_anula_cierre_orden_proceso

type variables

str_busqueda						istr_busq
str_mant								istr_mant
Str_info								lstr_info

uo_especie							iuo_Especie
uo_control_historico_proceso	iuo_historico

Integer    							ii_tiponum


end variables

forward prototypes
public subroutine habilitagrabar ()
public function boolean buscaorden (long al_orden, integer ai_tipo)
public function boolean buscaordenreproceso (long al_orden)
public function boolean historial ()
end prototypes

event ue_validapassword();str_mant	lstr_mant

lstr_mant.Argumento[1]	=	"Granel - Cierre de Proceso"
lstr_mant.Argumento[2]	=	gstr_paramplanta.Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public subroutine habilitagrabar ();
end subroutine

public function boolean buscaorden (long al_orden, integer ai_tipo);Integer li_especie, li_variedad,li_estado
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
   AND orpr_tipord	=	:ai_tipo
	AND orpr_numero	=	:al_orden
	AND clie_codigo	= 	:uo_SelCliente.Codigo;
		 
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

   IF li_estado = 1 THEN
   	sle_mensa.text    =  'Vigente'
	ELSEIF li_estado = 2 THEN
		sle_mensa.text    =  'Confir. Packing'
	ELSEIF li_estado = 3 THEN
		sle_mensa.text    =  'Cerrada'
	ELSEIF li_estado = 5 THEN
		sle_mensa.Text = "Cierre Web"
		Messagebox("Error", "Esta orden de proceso esta publicada a los productores, imposible modificar su estado", StopSign!)
		Return False
	ELSE
		sle_mensa.text = "Cerrada - Modificada en packing"
	END IF
	
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
	em_fecha.text		=	String(Date(ldt_Fecha))
	
	IF li_estado = 1 THEN
   	sle_mensa.text    =  'Vigente'
	ELSEIF li_estado = 2 THEN
		sle_mensa.text    =  'Confir. Packing'
	ELSE	
		sle_mensa.text    =  'Cerrada'
	END IF
	
END IF

RETURN lb_Retorno
end function

public function boolean historial ();Boolean			lb_retorno	=	True
Integer			ll_fila
DwItemStatus	li_dwitemstatus
Integer			li_tipord, li_codmod, li_tipmov
Long				ll_proceso
Date 				ld_fecmov
Time				lt_hormov
String 			ls_pcname

ll_fila = 1
	
li_tipord			=	ii_tiponum
ll_proceso			=	long(em_proceso.text)
ld_fecmov			=	Date(f_fechahora())
lt_hormov			=	Time(f_fechahora())
ls_pcname			=	gstr_us.computador
		
li_tipmov			=	8//anula cierre de orden
li_codmod			=	1

lb_retorno			=	iuo_historico.InsertaHistoria(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, li_tipord, &
																	ll_proceso, li_codmod, li_tipmov, &
																	ld_fecmov,  lt_hormov, ls_pcname, &
																	True, Sqlca)

Return lb_retorno
end function

on w_anula_cierre_orden_proceso.create
this.uo_SelCliente=create uo_SelCliente
this.uo_SelPlanta=create uo_SelPlanta
this.st_12=create st_12
this.st_9=create st_9
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
this.sle_mensa=create sle_mensa
this.st_5=create st_5
this.pb_salir=create pb_salir
this.pb_acepta=create pb_acepta
this.st_4=create st_4
this.st_2=create st_2
this.st_1=create st_1
this.st_titulo=create st_titulo
this.st_3=create st_3
this.r_1=create r_1
this.Control[]={this.uo_SelCliente,&
this.uo_SelPlanta,&
this.st_12,&
this.st_9,&
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
this.sle_mensa,&
this.st_5,&
this.pb_salir,&
this.pb_acepta,&
this.st_4,&
this.st_2,&
this.st_1,&
this.st_titulo,&
this.st_3,&
this.r_1}
end on

on w_anula_cierre_orden_proceso.destroy
destroy(this.uo_SelCliente)
destroy(this.uo_SelPlanta)
destroy(this.st_12)
destroy(this.st_9)
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
destroy(this.sle_mensa)
destroy(this.st_5)
destroy(this.pb_salir)
destroy(this.pb_acepta)
destroy(this.st_4)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.st_titulo)
destroy(this.st_3)
destroy(this.r_1)
end on

event open;Boolean	lb_Cerrar

If lb_Cerrar Then
	Close(This)
Else
	X	=	0
	Y	=	0
	
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCliente.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gstr_paramplanta.CodigoPlanta)
	
	em_fecha.text 			= 	string(today(),'dd/mm/yyy')
	sle_mensa.text			=	'Vigente'
	ddlb_tipoproc.text 		= 	'1. - Proceso'
	ii_tiponum 				= 	4
	
	iuo_Especie				=	Create uo_Especie
	iuo_historico			=	Create uo_control_historico_proceso
	
	IF NOT IsNull(gstr_paramplanta.Password) AND Trim(gstr_paramplanta.Password) <> '' THEN
		PostEvent("ue_validapassword")
	END IF
End If
end event

type uo_SelCliente from uo_seleccion_clientesprod within w_anula_cierre_orden_proceso
event destroy ( )
integer x = 590
integer y = 312
integer height = 96
integer taborder = 40
end type

on uo_SelCliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_SelPlanta from uo_seleccion_plantas within w_anula_cierre_orden_proceso
event destroy ( )
integer x = 590
integer y = 432
integer height = 96
integer taborder = 60
end type

on uo_SelPlanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_12 from statictext within w_anula_cierre_orden_proceso
integer x = 165
integer y = 328
integer width = 347
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Cliente"
boolean focusrectangle = false
end type

type st_9 from statictext within w_anula_cierre_orden_proceso
integer x = 169
integer y = 1356
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Estado"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type sle_prod from singlelineedit within w_anula_cierre_orden_proceso
integer x = 777
integer y = 928
integer width = 905
integer height = 104
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_codpro from singlelineedit within w_anula_cierre_orden_proceso
integer x = 590
integer y = 928
integer width = 174
integer height = 104
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_20 from statictext within w_anula_cierre_orden_proceso
integer x = 165
integer y = 948
integer width = 393
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type sle_varie from singlelineedit within w_anula_cierre_orden_proceso
integer x = 777
integer y = 1164
integer width = 905
integer height = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_codvar from singlelineedit within w_anula_cierre_orden_proceso
integer x = 590
integer y = 1164
integer width = 174
integer height = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_espe from singlelineedit within w_anula_cierre_orden_proceso
integer x = 777
integer y = 1044
integer width = 905
integer height = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_codesp from singlelineedit within w_anula_cierre_orden_proceso
integer x = 590
integer y = 1044
integer width = 174
integer height = 100
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type cb_1 from commandbutton within w_anula_cierre_orden_proceso
integer x = 1015
integer y = 692
integer width = 96
integer height = 80
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

//CHOOSE CASE ii_tiponum
//
//	CASE 4
		lstr_busq.argum[1] = string(uo_SelPlanta.Codigo)
		lstr_busq.argum[2] = "1"
		lstr_busq.argum[3] = string(ii_tiponum)
		lstr_busq.argum[6] = ""
		
		OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)
		
		lstr_busq	= Message.PowerObjectParm
		
		IF lstr_busq.argum[6] <> "" THEN
			em_proceso.text = lstr_busq.argum[6]
			IF NOT BuscaOrden(long(lstr_busq.argum[6]),ii_tiponum) THEN
				 em_proceso.text = ""
			END IF	
		END IF

//END CHOOSE

RETURN 1
end event

type em_fecha from editmask within w_anula_cierre_orden_proceso
integer x = 590
integer y = 808
integer width = 384
integer height = 96
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "none"
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_8 from statictext within w_anula_cierre_orden_proceso
integer x = 165
integer y = 824
integer width = 370
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_proceso from editmask within w_anula_cierre_orden_proceso
integer x = 590
integer y = 680
integer width = 384
integer height = 96
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 1090519039
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long ll_orden

ll_orden = long(this.text)
//IF ii_tiponum=4 OR ii_tiponum=8 THEN
	IF NOT BuscaOrden(ll_orden,ii_tiponum) THEN
		this.text=""
	END IF
//ELSE
//	IF NOT BuscaOrdenReProceso(ll_orden) THEN
//		this.text=""
//	END IF
//END IF	
end event

type st_7 from statictext within w_anula_cierre_orden_proceso
integer x = 165
integer y = 700
integer width = 370
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nº Proceso"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type ddlb_tipoproc from dropdownlistbox within w_anula_cierre_orden_proceso
integer x = 590
integer y = 552
integer width = 640
integer height = 360
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 1090519039
string text = "none"
string item[] = {"1. - Proceso","2. - Re- Proceso","3. - Re-Embalaje","4. - Pre-Proceso"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE Index
	CASE 2
		ii_tiponum	=	5
		
	CASE 3
		ii_tiponum	=	7
		
	CASE 4
		ii_tiponum	=	8
		
	CASE ELSE
		ii_tiponum	=	4
		
END CHOOSE

sle_mensa.text		=  ""
sle_codpro.text	=	""
sle_prod.text		=	""
sle_codesp.text	=	""
sle_espe.text		=	""
sle_codvar.text	=	""
sle_varie.text		=	""
em_proceso.text	=	""
end event

type st_6 from statictext within w_anula_cierre_orden_proceso
integer x = 165
integer y = 440
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_anula_cierre_orden_proceso
integer x = 590
integer y = 1356
integer width = 1184
integer height = 88
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_anula_cierre_orden_proceso
integer x = 87
integer y = 1332
integer width = 1737
integer height = 156
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_anula_cierre_orden_proceso
integer x = 1929
integer y = 752
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
alignment htextalign = right!
end type

event clicked;Close(Parent)
end event

type pb_acepta from picturebutton within w_anula_cierre_orden_proceso
event clicked pbm_bnclicked
integer x = 1929
integer y = 428
integer width = 302
integer height = 244
integer taborder = 50
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = right!
end type

event clicked;Integer		li_variedad, li_respuesta, li_especie
Long			fila, ll_numero
Date 			ld_fecha, ld_fechapru
String			ls_Mensaje
SetPointer(HourGlass!)

IF ii_tiponum = 4 THEN
	ls_Mensaje = "Proceso"
ELSEIF ii_tiponum = 8 THEN
	ls_Mensaje	=	"Pre Proceso"
END IF	

IF sle_mensa.text	<> "Cerrada" THEN
	MessageBox("Atención","La orden de " + ls_Mensaje + " no se encuentra cerrada.",Exclamation!)
	Return 1
END IF

IF IsNull(ii_tiponum) THEN
	MessageBox("Atención","Debe seleccionar un tipo de proceso",Exclamation!)
	Return 1
END IF

ll_numero = long(em_proceso.text)
IF isnull(ll_numero) or ll_numero =0 THEN
	MessageBox("Atención","Debe seleccionar un número de  " + ls_Mensaje + " .",Exclamation!)
	Return 1
END IF	

UPDATE dbo.spro_movtofrutagranenca  SET mfge_estmov = 2
 WHERE ( plde_codigo = :uo_SelPlanta.Codigo ) AND 
	    ( tpmv_codigo = 21 ) AND
   	 ( defg_tipdoc = :ii_tiponum ) AND  
       ( defg_docrel = :ll_numero )  AND
		 ( clie_codigo = :uo_SelCliente.Codigo );

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Actualización de Tabla de Movimiento de Fruta Encabezado")	

	sle_mensa.text	= "Error al Generar Datos."
	ROLLBACK;
	RETURN
	
END IF	

Update dbo.spro_ordenproceso SET orpr_estado = 2
 WHERE plde_codigo	=	:uo_SelPlanta.Codigo
   and orpr_tipord	=	:ii_tiponum
	and orpr_numero	=	:ll_numero 
	and clie_codigo 	=	:uo_SelCliente.Codigo;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Actualización de Tabla spro_ordenproceso" )
	sle_mensa.text	= "Error al Generar Datos."
	ROLLBACK;
ELSE
	IF Historial() THEN
		COMMIT;
		sle_mensa.text	= "Confir. Packing"
	ELSE
		sle_mensa.text	= "Error al Generar Datos."
		ROLLBACK;
	END IF
END IF	

SetPointer(Arrow!)
end event

type st_4 from statictext within w_anula_cierre_orden_proceso
integer x = 165
integer y = 1184
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Variedad"
boolean focusrectangle = false
end type

type st_2 from statictext within w_anula_cierre_orden_proceso
integer x = 165
integer y = 564
integer width = 393
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Tipo Proceso"
boolean focusrectangle = false
end type

type st_1 from statictext within w_anula_cierre_orden_proceso
integer x = 165
integer y = 1056
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_titulo from statictext within w_anula_cierre_orden_proceso
integer x = 82
integer y = 68
integer width = 1733
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
string text = "Anula Cierre de Orden de Proceso"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_3 from statictext within w_anula_cierre_orden_proceso
integer x = 91
integer y = 232
integer width = 1733
integer height = 1100
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type r_1 from rectangle within w_anula_cierre_orden_proceso
long linecolor = 12632256
integer linethickness = 4
integer x = 773
integer y = 612
integer width = 165
integer height = 144
end type

