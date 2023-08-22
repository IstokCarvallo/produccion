$PBExportHeader$w_info_cuadratura_productor.srw
$PBExportComments$Informe Existencia Sitio Revisión
forward
global type w_info_cuadratura_productor from w_para_informes
end type
type st_1 from statictext within w_info_cuadratura_productor
end type
type st_6 from statictext within w_info_cuadratura_productor
end type
type st_8 from statictext within w_info_cuadratura_productor
end type
type st_2 from statictext within w_info_cuadratura_productor
end type
type em_desde from editmask within w_info_cuadratura_productor
end type
type st_7 from statictext within w_info_cuadratura_productor
end type
type em_hasta from editmask within w_info_cuadratura_productor
end type
type gb_3 from groupbox within w_info_cuadratura_productor
end type
type st_3 from statictext within w_info_cuadratura_productor
end type
type em_proceso from editmask within w_info_cuadratura_productor
end type
type cb_buscaorden from commandbutton within w_info_cuadratura_productor
end type
type cbx_todosproc from checkbox within w_info_cuadratura_productor
end type
type gb_4 from groupbox within w_info_cuadratura_productor
end type
type st_4 from statictext within w_info_cuadratura_productor
end type
type st_5 from statictext within w_info_cuadratura_productor
end type
type ddlb_tipoproc from dropdownlistbox within w_info_cuadratura_productor
end type
type uo_selproductor from uo_seleccion_productor within w_info_cuadratura_productor
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_cuadratura_productor
end type
type uo_selplanta from uo_seleccion_plantas within w_info_cuadratura_productor
end type
end forward

global type w_info_cuadratura_productor from w_para_informes
integer x = 14
integer y = 32
integer width = 2363
integer height = 1840
string title = "CUADRATURA PRODUCTOR"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_6 st_6
st_8 st_8
st_2 st_2
em_desde em_desde
st_7 st_7
em_hasta em_hasta
gb_3 gb_3
st_3 st_3
em_proceso em_proceso
cb_buscaorden cb_buscaorden
cbx_todosproc cbx_todosproc
gb_4 gb_4
st_4 st_4
st_5 st_5
ddlb_tipoproc ddlb_tipoproc
uo_selproductor uo_selproductor
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
end type
global w_info_cuadratura_productor w_info_cuadratura_productor

type variables
str_busqueda istr_busq
str_mant istr_mant						

Integer 	ii_TipoOrden, il_ppre_numero
Date		id_fecini, id_fecter		
end variables

forward prototypes
public function boolean noexisteordenproceso (long al_orden)
public function boolean noexisteordenreproceso (long al_orden, integer ai_tipo)
end prototypes

public function boolean noexisteordenproceso (long al_orden);Integer	li_Especie, li_Estado, li_Variedad, li_Seguimto, li_Linea, li_periodo
String		ls_Seguimiento[3]={'Productor','Huerto','Cuartel'} 
Date  		ldt_Fecha
Boolean	lb_Retorno = True
Long		ll_Productor

SELECT	espe_codigo, orpr_estado, orpr_fecpro, prod_codigo,
			vari_codigo, orpr_niveld, line_codigo, pefr_codigo, 
			ppre_numero
	INTO	:li_Especie, :li_Estado, :ldt_Fecha, :ll_Productor, 
			:li_Variedad, :li_Seguimto, :li_Linea, :li_periodo, 
			:il_ppre_numero
	FROM	dbo.spro_ordenproceso
	WHERE	:uo_SelPlanta.Codigo in (-1,plde_codigo)
	AND	orpr_tipord	=	:ii_TipoOrden
	AND	orpr_numero	=	:al_Orden
	AND 	clie_codigo	= 	:uo_SelCliente.Codigo;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Spro_Ordenproceso")
ELSEIF sqlca.SQLCode = 0 THEN
		
	em_desde.text			=	String(ldt_Fecha,'dd/mm/yyyy')
	em_hasta.text			=	String(ldt_Fecha,'dd/mm/yyyy')

	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean noexisteordenreproceso (long al_orden, integer ai_tipo);Integer	li_Especie, li_Estado, li_Linea, li_Nula, li_Variedad, li_Seguimto, li_periodo
Date  		ldt_Fecha
Boolean	lb_Retorno = True
Long		ll_Productor, ll_ppre_numero

SetNull(li_Nula)

SELECT	espe_codigo, dinp_estado, dinp_fecdoc, line_codigo
	INTO	:li_Especie, :li_Estado, :ldt_Fecha, :li_Linea
	FROM	dbo.spro_doctointernopack
	WHERE	:uo_SelPlanta.Codigo in (-1,plde_codigo)
	AND	dinp_tipdoc	=	:ai_tipo
	AND	dinp_numero	=	:al_Orden;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_doctointernopack")
ELSEIF sqlca.SQLCode <> 100 THEN
	em_desde.text								=	String(Date(ldt_Fecha))
	em_hasta.text								=	String(Date(ldt_Fecha))
	em_hasta.Enabled			=	FALSE
	lb_Retorno					=	FALSE
END IF

RETURN lb_Retorno
end function

on w_info_cuadratura_productor.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_6=create st_6
this.st_8=create st_8
this.st_2=create st_2
this.em_desde=create em_desde
this.st_7=create st_7
this.em_hasta=create em_hasta
this.gb_3=create gb_3
this.st_3=create st_3
this.em_proceso=create em_proceso
this.cb_buscaorden=create cb_buscaorden
this.cbx_todosproc=create cbx_todosproc
this.gb_4=create gb_4
this.st_4=create st_4
this.st_5=create st_5
this.ddlb_tipoproc=create ddlb_tipoproc
this.uo_selproductor=create uo_selproductor
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_8
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_desde
this.Control[iCurrent+6]=this.st_7
this.Control[iCurrent+7]=this.em_hasta
this.Control[iCurrent+8]=this.gb_3
this.Control[iCurrent+9]=this.st_3
this.Control[iCurrent+10]=this.em_proceso
this.Control[iCurrent+11]=this.cb_buscaorden
this.Control[iCurrent+12]=this.cbx_todosproc
this.Control[iCurrent+13]=this.gb_4
this.Control[iCurrent+14]=this.st_4
this.Control[iCurrent+15]=this.st_5
this.Control[iCurrent+16]=this.ddlb_tipoproc
this.Control[iCurrent+17]=this.uo_selproductor
this.Control[iCurrent+18]=this.uo_selcliente
this.Control[iCurrent+19]=this.uo_selplanta
end on

on w_info_cuadratura_productor.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_6)
destroy(this.st_8)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.gb_3)
destroy(this.st_3)
destroy(this.em_proceso)
destroy(this.cb_buscaorden)
destroy(this.cbx_todosproc)
destroy(this.gb_4)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.ddlb_tipoproc)
destroy(this.uo_selproductor)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
end on

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	
	uo_SelCliente.Inicia(gi_CodExport)

	em_desde.Text				=	String(RelativeDate(Today(), -365))
	em_hasta.Text				=	String(Today())
	
	ii_TipoOrden	=	4
	
	ddlb_tipoproc.Text = 'Proceso'
	
	id_fecini		=	RelativeDate(Today(), -365)
	id_fecter		=	Today()
End If
end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_cuadratura_productor
end type

type st_computador from w_para_informes`st_computador within w_info_cuadratura_productor
end type

type st_usuario from w_para_informes`st_usuario within w_info_cuadratura_productor
end type

type st_temporada from w_para_informes`st_temporada within w_info_cuadratura_productor
end type

type p_logo from w_para_informes`p_logo within w_info_cuadratura_productor
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_cuadratura_productor
integer width = 1618
string text = "Cuadratura Productor"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_cuadratura_productor
integer x = 1938
integer y = 1104
integer taborder = 130
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_especie, li_variedad, li_etiqueta, li_consplanta,&
			li_consproductor, li_consvariedad, li_consembalaje, li_consetiqueta,li_conscalidad,&
			li_conspacking, li_packing, li_zona
String		texto_desde, texto_hasta, texto_fecha, ls_null
Long		ll_Orden

SetNull(ls_null)

istr_info.titulo	= 'CUADRATURA PRODUCTOR'	


OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_cuadra_productor"
vinf.dw_1.SetTransObject(sqlca)

IF ii_TipoOrden = 0 or cbx_todosproc.Checked THEN
	ll_Orden = -1
ELSE
	ll_Orden		=	Long(em_Proceso.text)
	
	IF IsNull(ll_Orden) THEN
		MessageBox("Error de Datos", "Falta el Ingreso de un Número de Orden.")
		
		RETURN
	END IF	
END IF

IF IsNull(ii_TipoOrden) THEN
	MessageBox("Error de Datos", "Falta el Ingreso del Tipo de Proceso.")
	RETURN
END IF

texto_desde	=  f_fecha_texto(String(id_fecini), 1)
texto_hasta		=	f_fecha_texto(String(id_fecter), 1)
texto_fecha		=	"Desde El :  " + texto_desde + "   Hasta El :  " + texto_hasta

IF IsNull(id_fecini) 	OR IsNull(id_fecter)THEN
		MessageBox("Error", "Debe Ingresar todos los parametros para el informe", StopSign!)
		return 1
END IF

fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, uo_SelProductor.Codigo, id_fecini, id_fecter, ii_TipoOrden, ll_Orden)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("fechas.text = '" + texto_fecha + "'")
	vinf.dw_1.Modify("t_cliente.text = '" + uo_SelCliente.Nombre + "'")		
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_cuadratura_productor
integer x = 1938
integer y = 1376
integer taborder = 140
end type

type st_1 from statictext within w_info_cuadratura_productor
integer x = 315
integer y = 680
integer width = 238
integer height = 76
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

type st_6 from statictext within w_info_cuadratura_productor
integer x = 315
integer y = 476
integer width = 233
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
string text = "Cliente"
boolean focusrectangle = false
end type

type st_8 from statictext within w_info_cuadratura_productor
integer x = 315
integer y = 888
integer width = 329
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
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_cuadratura_productor
integer x = 315
integer y = 1464
integer width = 229
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
string text = "Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_cuadratura_productor
integer x = 617
integer y = 1448
integer width = 407
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;istr_mant.argumento[9]	=	This.Text
id_fecini					= 	Date(this.Text)
end event

type st_7 from statictext within w_info_cuadratura_productor
integer x = 1061
integer y = 1464
integer width = 279
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
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_cuadratura_productor
integer x = 1330
integer y = 1448
integer width = 407
integer height = 96
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;istr_mant.argumento[10]	=	This.Text
id_fecter					= 	Date(this.Text)
end event

type gb_3 from groupbox within w_info_cuadratura_productor
integer x = 283
integer y = 972
integer width = 1531
integer height = 400
integer taborder = 150
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Proceso"
end type

type st_3 from statictext within w_info_cuadratura_productor
integer x = 315
integer y = 1228
integer width = 347
integer height = 72
boolean bringtotop = true
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

type em_proceso from editmask within w_info_cuadratura_productor
integer x = 672
integer y = 1224
integer width = 384
integer height = 84
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 1090519039
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long ll_orden

ll_orden = long(this.text)

CHOOSE CASE ii_TipoOrden
	CASE 4, 8
		IF NoExisteOrdenProceso(ll_orden) THEN
			This.Text=""
		END IF
		
	CASE 5, 6
		IF NoExisteOrdenReProceso(ll_orden,ii_TipoOrden) THEN
			this.Text=""
		END IF
	
END CHOOSE
end event

type cb_buscaorden from commandbutton within w_info_cuadratura_productor
integer x = 1088
integer y = 1228
integer width = 91
integer height = 76
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "..."
end type

event clicked;Str_busqueda	lstr_busq

CHOOSE CASE ii_TipoOrden
	CASE 4
		lstr_busq.argum[1]	=	String(uo_SelPlanta.Codigo)
		lstr_busq.argum[2]	=	"1"
		lstr_busq.argum[3]	=	string(ii_TipoOrden)
		lstr_busq.argum[6]	=	""
		
		OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)
		
		lstr_busq	= Message.PowerObjectParm
		
		IF lstr_busq.argum[6] <> "" THEN
			em_proceso.text	=	lstr_busq.argum[6]
			
			IF NoExisteOrdenProceso(Long(lstr_busq.argum[6])) THEN
				 em_proceso.text=""
			END IF	
		END IF
		
		RETURN 1
	
	CASE 5
		
		lstr_Busq.Argum[1]	=	'5'	// Tipo Orden

		OpenWithParm(w_busqueda_doctointernopack, lstr_Busq)

		lstr_Busq	= Message.PowerObjectParm

		IF lstr_Busq.Argum[2] <> "" THEN
			em_proceso.text	=	lstr_busq.argum[3]
			IF NoExisteOrdenReProceso(Long(lstr_Busq.Argum[3]),5) THEN
				em_proceso.Text=""
			END IF
		END IF
		
	CASE 6
		
		lstr_Busq.Argum[1]	=	'6'	// Tipo Orden

		OpenWithParm(w_busqueda_doctointernopack, lstr_Busq)

		lstr_Busq	= Message.PowerObjectParm

		IF lstr_Busq.Argum[2] <> "" THEN
			em_proceso.text	=	lstr_busq.argum[3]
			IF NoExisteOrdenReProceso(Long(lstr_Busq.Argum[3]),6) THEN
				em_proceso.Text=""
			END IF
		END IF
	
END CHOOSE
end event

type cbx_todosproc from checkbox within w_info_cuadratura_productor
integer x = 1221
integer y = 1232
integer width = 343
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;String ls_Null
SetNull(ls_Null)

IF This.Checked THEN
	em_proceso.Text							=	''
	cb_buscaorden.Enabled					=	False
	em_proceso.Enabled						=	False
	
ELSE
	cb_buscaorden.Enabled					=	True
	em_proceso.Enabled						=	True
	
END IF








end event

type gb_4 from groupbox within w_info_cuadratura_productor
integer x = 283
integer y = 1376
integer width = 1531
integer height = 200
integer taborder = 160
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Proceso"
end type

type st_4 from statictext within w_info_cuadratura_productor
integer x = 242
integer y = 420
integer width = 1618
integer height = 1192
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_cuadratura_productor
integer x = 315
integer y = 1084
integer width = 347
integer height = 72
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

type ddlb_tipoproc from dropdownlistbox within w_info_cuadratura_productor
integer x = 672
integer y = 1072
integer width = 640
integer height = 368
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 8388608
long backcolor = 16777215
string text = "none"
boolean sorted = false
boolean vscrollbar = true
string item[] = {"Todos","Proceso","Re-Proceso","Re-Embalaje","Pre-Proceso"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE index
		
	CASE 1
		ii_TipoOrden				=	-1
		em_proceso.Enabled		=	False
		cb_buscaorden.Enabled	=	False
		cbx_todosproc.Enabled	=	False
		em_desde.Enabled			=	True
		//em_hasta.Enabled			=	True
		//em_hasta.Visible			=	True
		st_2.Visible			=	True
		cbx_todosproc.Checked	=	True
		cbx_todosproc.TriggerEvent(Clicked!)
		
	CASE 2,3,4
		ii_TipoOrden	=	index + 2
		em_proceso.Enabled		=	True
		em_proceso.Text			=	''
		cb_buscaorden.Enabled	=	True
		cbx_todosproc.Checked	=	False
		cbx_todosproc.Enabled	=	True
		//em_hasta.Visible			=	False
		//st_2.Visible				=	False
		cbx_todosproc.TriggerEvent(Clicked!)
		
	CASE 5
		ii_TipoOrden				=	8
		em_proceso.Enabled		=	True
		em_proceso.Text			=	''
		cb_buscaorden.Enabled	=	True
		cbx_todosproc.Checked	=	False
		cbx_todosproc.Enabled	=	True
		//em_hasta.Visible			=	False
		//st_2.Visible			=	False
		cbx_todosproc.TriggerEvent(Clicked!)				
END CHOOSE
end event

type uo_selproductor from uo_seleccion_productor within w_info_cuadratura_productor
event destroy ( )
integer x = 667
integer y = 796
integer taborder = 30
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_info_cuadratura_productor
event destroy ( )
integer x = 667
integer y = 452
integer height = 96
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_cuadratura_productor
event destroy ( )
integer x = 667
integer y = 588
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

