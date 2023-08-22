$PBExportHeader$w_info_resultado_prodcont.srw
$PBExportComments$Resultado de Proceso Kilos
forward
global type w_info_resultado_prodcont from w_para_informes
end type
type cb_buscaorden from commandbutton within w_info_resultado_prodcont
end type
type em_fecdes from editmask within w_info_resultado_prodcont
end type
type st_fecha from statictext within w_info_resultado_prodcont
end type
type em_proceso from editmask within w_info_resultado_prodcont
end type
type st_7 from statictext within w_info_resultado_prodcont
end type
type st_6 from statictext within w_info_resultado_prodcont
end type
type st_2 from statictext within w_info_resultado_prodcont
end type
type ddlb_tipoproc from dropdownlistbox within w_info_resultado_prodcont
end type
type cbx_todosproc from checkbox within w_info_resultado_prodcont
end type
type em_fecter from editmask within w_info_resultado_prodcont
end type
type st_12 from statictext within w_info_resultado_prodcont
end type
type st_1 from statictext within w_info_resultado_prodcont
end type
type st_3 from statictext within w_info_resultado_prodcont
end type
type cbx_todfec from checkbox within w_info_resultado_prodcont
end type
type gb_3 from groupbox within w_info_resultado_prodcont
end type
type st_fecter from statictext within w_info_resultado_prodcont
end type
type st_9 from statictext within w_info_resultado_prodcont
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_resultado_prodcont
end type
type uo_selproductor from uo_seleccion_productor within w_info_resultado_prodcont
end type
type uo_selplanta from uo_seleccion_plantas within w_info_resultado_prodcont
end type
end forward

global type w_info_resultado_prodcont from w_para_informes
integer width = 2587
integer height = 1772
string title = "Informe Productor"
cb_buscaorden cb_buscaorden
em_fecdes em_fecdes
st_fecha st_fecha
em_proceso em_proceso
st_7 st_7
st_6 st_6
st_2 st_2
ddlb_tipoproc ddlb_tipoproc
cbx_todosproc cbx_todosproc
em_fecter em_fecter
st_12 st_12
st_1 st_1
st_3 st_3
cbx_todfec cbx_todfec
gb_3 gb_3
st_fecter st_fecter
st_9 st_9
uo_selcliente uo_selcliente
uo_selproductor uo_selproductor
uo_selplanta uo_selplanta
end type
global w_info_resultado_prodcont w_info_resultado_prodcont

type variables

Integer    ii_TipoOrden, il_ppre_numero

end variables

forward prototypes
public function boolean noexisteordenreproceso (long al_orden, integer ai_tipo)
public function boolean noexisteordenproceso (long al_orden)
end prototypes

public function boolean noexisteordenreproceso (long al_orden, integer ai_tipo);Integer	li_Especie, li_Estado, li_Linea, li_Nula, li_Variedad, li_Seguimto, li_periodo
Date  		ldt_Fecha
Boolean	lb_Retorno = True
Long		ll_Productor, ll_ppre_numero

SetNull(li_Nula)


SELECT	espe_codigo, dinp_estado, dinp_fecdoc, line_codigo
	INTO	:li_Especie, :li_Estado, :ldt_Fecha, :li_Linea
	FROM	dbo.spro_doctointernopack
	WHERE	plde_codigo	=	:uo_SelPlanta.Codigo
	AND	dinp_tipdoc	=	:ai_tipo
	AND	dinp_numero	=	:al_Orden;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_doctointernopack")
ELSEIF sqlca.SQLCode <> 100 THEN
	em_fecdes.text				=	String(Date(ldt_Fecha))
	em_fecter.text				=	String(Date(ldt_Fecha))
	em_fecdes.Enabled		=	FALSE
	lb_Retorno					=	FALSE
END IF

RETURN lb_Retorno
end function

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
	WHERE	plde_codigo	=	:uo_SelPlanta.Codigo
	AND	orpr_tipord	=	:ii_TipoOrden
	AND	orpr_numero	=	:al_Orden
	AND 	clie_codigo	= 	:uo_SelCliente.Codigo;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Spro_Ordenproceso")
ELSEIF sqlca.SQLCode = 0 THEN
		
	em_fecdes.text			=	String(ldt_Fecha,'dd/mm/yyyy')
	em_fecter.text			=	String(ldt_Fecha,'dd/mm/yyyy')

	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

on w_info_resultado_prodcont.create
int iCurrent
call super::create
this.cb_buscaorden=create cb_buscaorden
this.em_fecdes=create em_fecdes
this.st_fecha=create st_fecha
this.em_proceso=create em_proceso
this.st_7=create st_7
this.st_6=create st_6
this.st_2=create st_2
this.ddlb_tipoproc=create ddlb_tipoproc
this.cbx_todosproc=create cbx_todosproc
this.em_fecter=create em_fecter
this.st_12=create st_12
this.st_1=create st_1
this.st_3=create st_3
this.cbx_todfec=create cbx_todfec
this.gb_3=create gb_3
this.st_fecter=create st_fecter
this.st_9=create st_9
this.uo_selcliente=create uo_selcliente
this.uo_selproductor=create uo_selproductor
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_buscaorden
this.Control[iCurrent+2]=this.em_fecdes
this.Control[iCurrent+3]=this.st_fecha
this.Control[iCurrent+4]=this.em_proceso
this.Control[iCurrent+5]=this.st_7
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.st_2
this.Control[iCurrent+8]=this.ddlb_tipoproc
this.Control[iCurrent+9]=this.cbx_todosproc
this.Control[iCurrent+10]=this.em_fecter
this.Control[iCurrent+11]=this.st_12
this.Control[iCurrent+12]=this.st_1
this.Control[iCurrent+13]=this.st_3
this.Control[iCurrent+14]=this.cbx_todfec
this.Control[iCurrent+15]=this.gb_3
this.Control[iCurrent+16]=this.st_fecter
this.Control[iCurrent+17]=this.st_9
this.Control[iCurrent+18]=this.uo_selcliente
this.Control[iCurrent+19]=this.uo_selproductor
this.Control[iCurrent+20]=this.uo_selplanta
end on

on w_info_resultado_prodcont.destroy
call super::destroy
destroy(this.cb_buscaorden)
destroy(this.em_fecdes)
destroy(this.st_fecha)
destroy(this.em_proceso)
destroy(this.st_7)
destroy(this.st_6)
destroy(this.st_2)
destroy(this.ddlb_tipoproc)
destroy(this.cbx_todosproc)
destroy(this.em_fecter)
destroy(this.st_12)
destroy(this.st_1)
destroy(this.st_3)
destroy(this.cbx_todfec)
destroy(this.gb_3)
destroy(this.st_fecter)
destroy(this.st_9)
destroy(this.uo_selcliente)
destroy(this.uo_selproductor)
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
	uo_SelPlanta.Seleccion(False, False)
	uo_SelProductor.Seleccion(True, False)
	
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gstr_ParamPlanta.CodigoPlanta)

	em_fecdes.text	=	String(Date(Today()),'dd/mm/yyyy')
	em_fecter.text	=	String(Date(Today()),'dd/mm/yyyy')
	
	ii_TipoOrden	=	4
	
	ddlb_tipoproc.Text = 'Proceso'
	cbx_todosproc.TriggerEvent("Clicked")
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_resultado_prodcont
end type

type st_computador from w_para_informes`st_computador within w_info_resultado_prodcont
end type

type st_usuario from w_para_informes`st_usuario within w_info_resultado_prodcont
end type

type st_temporada from w_para_informes`st_temporada within w_info_resultado_prodcont
end type

type p_logo from w_para_informes`p_logo within w_info_resultado_prodcont
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_resultado_prodcont
integer width = 1755
string text = "Informe Resultado Productor"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resultado_prodcont
integer x = 2107
integer y = 1072
integer taborder = 80
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila, ll_Orden
Integer  	li_Especie, li_Variedad, li_ConsPredio, li_TipoInforme, li_Linea, li_Periodo, li_ConsFecha = 0, li_contratista
Date		ld_fecdes,ld_fecter
Dec{2}   ld_peso

ld_Fecdes		=	Date(left(em_fecdes.Text, 10))
ld_Fecter		=	Date(left(em_fecter.Text, 10))

IF IsNull(ii_TipoOrden) THEN
	MessageBox("Error de Datos", "Falta el Ingreso del Tipo de Proceso.")
	RETURN
END IF

IF ii_TipoOrden = 0 or cbx_todosproc.Checked THEN
	ll_Orden = -1
ELSE
	ll_Orden		=	Long(em_Proceso.text)
	
	IF IsNull(ll_Orden) THEN
		MessageBox("Error de Datos", "Falta el Ingreso de un Número de Orden.")
		
		RETURN
	END IF	
END IF

IF cbx_todfec.Checked THEN
	ld_fecdes = Date('19000101')
	ld_fecter = Date(Today())
END IF

istr_info.Titulo		=	"Informe Proceso en Kilos al Contratista"

li_contratista			=	-1
li_Especie				=	-1
li_Variedad				=	-1
li_Linea					=	-1
li_Periodo				=	-1

istr_info.Copias		=	1

OpenWithParm(vinf, istr_info)

Vinf.dw_1.DataObject =	"dw_info_procesokilos_prodcont"
	
Vinf.dw_1.SetTransObject(sqlca)
ll_Fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo,ii_TipoOrden,ll_orden, uo_SelProductor.Codigo,&
									 li_Especie,li_Variedad,li_Linea,li_Periodo,ld_fecdes,&
									 ld_fecter,li_ConsPredio, li_ConsFecha,li_contratista)

 
IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	ParamTemporada(gstr_paramtempo)
	vinf.dw_1.Modify("nombre_temporada.text = '" + gstr_paramtempo.nombre + "'")
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_resultado_prodcont
integer x = 2107
integer y = 1336
integer taborder = 90
end type

type cb_buscaorden from commandbutton within w_info_resultado_prodcont
integer x = 1129
integer y = 1116
integer width = 91
integer height = 76
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

type em_fecdes from editmask within w_info_resultado_prodcont
integer x = 667
integer y = 1388
integer width = 480
integer height = 84
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_fecha from statictext within w_info_resultado_prodcont
integer x = 443
integer y = 1396
integer width = 197
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_proceso from editmask within w_info_resultado_prodcont
integer x = 745
integer y = 1112
integer width = 384
integer height = 84
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
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

type st_7 from statictext within w_info_resultado_prodcont
integer x = 311
integer y = 1116
integer width = 347
integer height = 72
boolean bringtotop = true
integer textsize = -9
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

type st_6 from statictext within w_info_resultado_prodcont
integer x = 311
integer y = 588
integer width = 347
integer height = 68
boolean bringtotop = true
integer textsize = -9
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

type st_2 from statictext within w_info_resultado_prodcont
integer x = 311
integer y = 752
integer width = 347
integer height = 72
boolean bringtotop = true
integer textsize = -9
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

type ddlb_tipoproc from dropdownlistbox within w_info_resultado_prodcont
integer x = 745
integer y = 740
integer width = 640
integer height = 368
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 1090519039
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
		em_fecdes.Enabled			=	True
		em_fecter.Enabled			=	True
		em_fecter.Visible			=	True
		st_fecter.Visible			=	True
		cbx_todosproc.Checked	=	True
		cbx_todosproc.TriggerEvent(Clicked!)
		
	CASE 2,3,4
		ii_TipoOrden	=	index + 2
		em_proceso.Enabled		=	True
		em_proceso.Text			=	''
		cb_buscaorden.Enabled	=	True
		cbx_todosproc.Checked	=	False
		cbx_todosproc.Enabled	=	True
		em_fecter.Visible			=	False
		st_fecter.Visible			=	False
		cbx_todosproc.TriggerEvent(Clicked!)
		
	CASE 5
		ii_TipoOrden				=	8
		em_proceso.Enabled		=	True
		em_proceso.Text			=	''
		cb_buscaorden.Enabled	=	True
		cbx_todosproc.Checked	=	False
		cbx_todosproc.Enabled	=	True
		em_fecter.Visible			=	False
		st_fecter.Visible			=	False
		cbx_todosproc.TriggerEvent(Clicked!)				
END CHOOSE
end event

type cbx_todosproc from checkbox within w_info_resultado_prodcont
integer x = 1234
integer y = 1120
integer width = 270
integer height = 72
boolean bringtotop = true
integer textsize = -9
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

type em_fecter from editmask within w_info_resultado_prodcont
integer x = 1417
integer y = 1388
integer width = 480
integer height = 84
integer taborder = 70
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_12 from statictext within w_info_resultado_prodcont
integer x = 311
integer y = 468
integer width = 347
integer height = 68
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Cliente"
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_resultado_prodcont
integer x = 311
integer y = 968
integer width = 347
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_resultado_prodcont
integer x = 242
integer y = 416
integer width = 1755
integer height = 292
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

type cbx_todfec from checkbox within w_info_resultado_prodcont
integer x = 718
integer y = 1280
integer width = 270
integer height = 96
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
end type

event clicked;IF This.Checked THEN
	em_fecdes.Enabled 	  	= False
	em_fecter.Enabled 	  	= False
ELSE
	em_fecdes.Enabled 	 	= True
	em_fecter.Enabled 	 	= True
END IF
end event

type gb_3 from groupbox within w_info_resultado_prodcont
integer x = 274
integer y = 1224
integer width = 1682
integer height = 276
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
end type

type st_fecter from statictext within w_info_resultado_prodcont
integer x = 1221
integer y = 1400
integer width = 251
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Hasta"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_resultado_prodcont
integer x = 242
integer y = 708
integer width = 1755
integer height = 820
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

event clicked;//IF This.Checked THEN
//	dw_contratista.Enabled = False
//	cbx_concont.Checked 	  = False	
//ELSE
//	dw_contratista.Enabled = True
//	cbx_concont.Checked 	  = False
//END IF
end event

type uo_selcliente from uo_seleccion_clientesprod within w_info_resultado_prodcont
event destroy ( )
integer x = 745
integer y = 452
integer height = 100
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_resultado_prodcont
event destroy ( )
integer x = 745
integer y = 868
integer taborder = 20
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_resultado_prodcont
event destroy ( )
integer x = 745
integer y = 572
integer height = 100
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

