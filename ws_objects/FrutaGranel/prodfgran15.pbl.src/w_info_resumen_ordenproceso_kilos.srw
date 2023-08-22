$PBExportHeader$w_info_resumen_ordenproceso_kilos.srw
$PBExportComments$Resultado de Proceso Kilos
forward
global type w_info_resumen_ordenproceso_kilos from w_para_informes
end type
type cb_buscaorden from commandbutton within w_info_resumen_ordenproceso_kilos
end type
type em_desde from editmask within w_info_resumen_ordenproceso_kilos
end type
type st_fecha from statictext within w_info_resumen_ordenproceso_kilos
end type
type em_proceso from editmask within w_info_resumen_ordenproceso_kilos
end type
type st_7 from statictext within w_info_resumen_ordenproceso_kilos
end type
type st_6 from statictext within w_info_resumen_ordenproceso_kilos
end type
type st_2 from statictext within w_info_resumen_ordenproceso_kilos
end type
type ddlb_tipoproc from dropdownlistbox within w_info_resumen_ordenproceso_kilos
end type
type st_1 from statictext within w_info_resumen_ordenproceso_kilos
end type
type st_4 from statictext within w_info_resumen_ordenproceso_kilos
end type
type st_10 from statictext within w_info_resumen_ordenproceso_kilos
end type
type em_hasta from editmask within w_info_resumen_ordenproceso_kilos
end type
type st_12 from statictext within w_info_resumen_ordenproceso_kilos
end type
type st_fecter from statictext within w_info_resumen_ordenproceso_kilos
end type
type st_3 from statictext within w_info_resumen_ordenproceso_kilos
end type
type st_15 from statictext within w_info_resumen_ordenproceso_kilos
end type
type cbx_todosproc from checkbox within w_info_resumen_ordenproceso_kilos
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_resumen_ordenproceso_kilos
end type
type uo_selplanta from uo_seleccion_plantas within w_info_resumen_ordenproceso_kilos
end type
type uo_selespecie from uo_seleccion_especie within w_info_resumen_ordenproceso_kilos
end type
type uo_selproductor from uo_seleccion_productor within w_info_resumen_ordenproceso_kilos
end type
type uo_selvariedades from uo_seleccion_variedad within w_info_resumen_ordenproceso_kilos
end type
end forward

global type w_info_resumen_ordenproceso_kilos from w_para_informes
integer width = 3008
integer height = 1908
string title = "Resumen Resultado de Proceso"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
cb_buscaorden cb_buscaorden
em_desde em_desde
st_fecha st_fecha
em_proceso em_proceso
st_7 st_7
st_6 st_6
st_2 st_2
ddlb_tipoproc ddlb_tipoproc
st_1 st_1
st_4 st_4
st_10 st_10
em_hasta em_hasta
st_12 st_12
st_fecter st_fecter
st_3 st_3
st_15 st_15
cbx_todosproc cbx_todosproc
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
uo_selespecie uo_selespecie
uo_selproductor uo_selproductor
uo_selvariedades uo_selvariedades
end type
global w_info_resumen_ordenproceso_kilos w_info_resumen_ordenproceso_kilos

type variables
Integer    ii_TipoOrden
end variables

forward prototypes
public function boolean noexisteordenreproceso (long al_orden, integer ai_tipo)
public function boolean noexisteordenproceso (long al_orden)
end prototypes

public function boolean noexisteordenreproceso (long al_orden, integer ai_tipo);Integer	li_Especie, li_Planta, li_Estado, li_Linea, li_Nula, li_cliente,&
			li_Variedad, li_Seguimto, li_periodo
Date  		ldt_Fecha
Boolean	lb_Retorno = True
Long		ll_ppre_numero

SetNull(li_Nula)

li_Planta	=	uo_SelPlanta.Codigo
li_cliente = 	uo_SelCliente.Codigo

SELECT	espe_codigo, dinp_estado, dinp_fecdoc, line_codigo
	INTO	:li_Especie, :li_Estado, :ldt_Fecha, :li_Linea
	FROM	dbo.spro_doctointernopack
	WHERE	plde_codigo	=	:li_Planta
	AND	dinp_tipdoc		=	:ai_tipo
	AND	dinp_numero	=	:al_Orden;
		 
If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla spro_doctointernopack")
ElseIf sqlca.SQLCode <> 100 Then	
	
	uo_SelVariedades.Filtra(li_Especie)
	
	uo_SelEspecie.dw_Seleccion.Object.Codigo[1]	=	li_Especie
	uo_SelVariedades.dw_Seleccion.Object.Codigo[1]	=	li_Nula
	uo_SelEspecie.Codigo			=	li_Especie
	uo_SelVariedades.Codigo	=	li_Nula
	
	em_Desde.text								=	String(Date(ldt_Fecha))
	em_Hasta.text								=	String(Date(ldt_Fecha))

	uo_SelEspecie.Bloquear(True)
	uo_SelVariedades.Bloquear(True)
	uo_SelVariedades.Todos(True)
	uo_SelProductor.Bloquear(True)
	uo_SelProductor.Todos(True)
		
	em_Desde.Enabled		=	False
	lb_Retorno					=	False
End If

Return lb_Retorno
end function

public function boolean noexisteordenproceso (long al_orden);Integer	li_Especie, li_Estado, li_Variedad, li_Seguimto, li_Linea, li_periodo, li_turno, li_cont, li_Cliente
String		ls_Seguimiento[3]={'Productor','Huerto','Cuartel'} 
Date  		ldt_Fecha
Boolean	lb_Retorno = True
Long		ll_Productor


SELECT espe_codigo, orpr_estado, orpr_fecpro, prod_codigo,
		vari_codigo, orpr_niveld, line_codigo, pefr_codigo, 
		IsNull(orpr_nrotur, 0), cont_codigo, clie_codigo
 INTO	:li_Especie, :li_Estado, :ldt_Fecha, :ll_Productor, 
		:li_Variedad, :li_Seguimto, :li_Linea, :li_periodo, 
		:li_turno, :li_cont, :li_Cliente
 FROM	dbo.spro_ordenproceso
WHERE	plde_codigo	=	:uo_SelPlanta.Codigo
  AND		orpr_tipord	=	:ii_TipoOrden
  AND		orpr_numero	=	:al_Orden
  AND		:uo_SelCliente.Codigo in(-1, clie_codigo);

If sqlca.SQLCode = -1 Then
	F_errorbasedatos(sqlca,"Lectura tabla Spro_Ordenproceso")
ElseIf sqlca.SQLCode = 0 Then
	If Not manbin_especie(uo_SelPlanta.Codigo, li_Especie, True, sqlca) Then
		Return lb_retorno
	End If
	
	uo_SelVariedades.Filtra(li_Especie)
	uo_SelProductor.Todos(False)
	uo_SelVariedades.Todos(False)
	uo_SelCliente.Todos(False)
	
	uo_SelEspecie.Codigo 									=	li_Especie
	uo_SelEspecie.dw_Seleccion.Object.Codigo[1]		=	li_Especie
	uo_SelVariedades.Codigo 								=	li_Variedad
	uo_SelVariedades.dw_Seleccion.Object.Codigo[1]	=	li_Variedad
	uo_SelProductor.Codigo 									=	ll_Productor
	uo_SelProductor.dw_Seleccion.Object.Codigo[1]	=	ll_Productor
	uo_SelCliente.Codigo 										=	li_Cliente
	uo_SelCliente.dw_Seleccion.Object.Codigo[1]		=	li_Cliente

	uo_SelEspecie.Bloquear(True)
	uo_SelVariedades.Bloquear(True)
	uo_SelProductor.Bloquear(True)
	uo_SelCliente.Bloquear(True)
	
	em_Desde.Enabled		=	False
	
	em_Desde.Text			=	String(ldt_Fecha,'dd/mm/yyyy')
	em_Hasta.Text			=	String(ldt_Fecha,'dd/mm/yyyy')
		
	lb_Retorno	=	False
End If

RETURN lb_Retorno
end function

on w_info_resumen_ordenproceso_kilos.create
int iCurrent
call super::create
this.cb_buscaorden=create cb_buscaorden
this.em_desde=create em_desde
this.st_fecha=create st_fecha
this.em_proceso=create em_proceso
this.st_7=create st_7
this.st_6=create st_6
this.st_2=create st_2
this.ddlb_tipoproc=create ddlb_tipoproc
this.st_1=create st_1
this.st_4=create st_4
this.st_10=create st_10
this.em_hasta=create em_hasta
this.st_12=create st_12
this.st_fecter=create st_fecter
this.st_3=create st_3
this.st_15=create st_15
this.cbx_todosproc=create cbx_todosproc
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.uo_selespecie=create uo_selespecie
this.uo_selproductor=create uo_selproductor
this.uo_selvariedades=create uo_selvariedades
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_buscaorden
this.Control[iCurrent+2]=this.em_desde
this.Control[iCurrent+3]=this.st_fecha
this.Control[iCurrent+4]=this.em_proceso
this.Control[iCurrent+5]=this.st_7
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.st_2
this.Control[iCurrent+8]=this.ddlb_tipoproc
this.Control[iCurrent+9]=this.st_1
this.Control[iCurrent+10]=this.st_4
this.Control[iCurrent+11]=this.st_10
this.Control[iCurrent+12]=this.em_hasta
this.Control[iCurrent+13]=this.st_12
this.Control[iCurrent+14]=this.st_fecter
this.Control[iCurrent+15]=this.st_3
this.Control[iCurrent+16]=this.st_15
this.Control[iCurrent+17]=this.cbx_todosproc
this.Control[iCurrent+18]=this.uo_selcliente
this.Control[iCurrent+19]=this.uo_selplanta
this.Control[iCurrent+20]=this.uo_selespecie
this.Control[iCurrent+21]=this.uo_selproductor
this.Control[iCurrent+22]=this.uo_selvariedades
end on

on w_info_resumen_ordenproceso_kilos.destroy
call super::destroy
destroy(this.cb_buscaorden)
destroy(this.em_desde)
destroy(this.st_fecha)
destroy(this.em_proceso)
destroy(this.st_7)
destroy(this.st_6)
destroy(this.st_2)
destroy(this.ddlb_tipoproc)
destroy(this.st_1)
destroy(this.st_4)
destroy(this.st_10)
destroy(this.em_hasta)
destroy(this.st_12)
destroy(this.st_fecter)
destroy(this.st_3)
destroy(this.st_15)
destroy(this.cbx_todosproc)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.uo_selespecie)
destroy(this.uo_selproductor)
destroy(this.uo_selvariedades)
end on

event open;call super::open;Boolean lb_Cerrar

If IsNull(uo_SelPlanta.codigo) Then lb_Cerrar = True
If IsNull(uo_SelCliente.codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.codigo) Then lb_Cerrar = True
If IsNull(uo_SelVariedades.codigo) Then lb_Cerrar = True
If IsNull(uo_SelProductor.codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	
	uo_SelCliente.Seleccion(True, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelVariedades.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelProductor.Filtra(-1)
	
	uo_SelPlanta.Inicia(gstr_paramplanta.CodigoPlanta)
	uo_SelEspecie.Inicia(gstr_paramplanta.CodigoEspecie)

	em_Desde.Text	=	String(Date(Today()),'dd/mm/yyyy')
	em_Hasta.Text	=	String(Date(Today()),'dd/mm/yyyy')
	
	ii_TipoOrden	=	4
	
	ddlb_tipoproc.Text = 'Proceso'
End if
end event

event resize;Integer		li_posi_y, li_objeto

p_Logo.x						=	0
p_Logo.y						=	0
st_Temporada.x				=	p_Logo.Width + 5
st_Temporada.y				=	0
st_Temporada.Height		=	p_Logo.Height
st_Temporada.Width		=	This.Width - p_Logo.Width - 80
st_Usuario.x				=	p_Logo.Width + 5
st_Usuario.y				=	72
st_Usuario.Height			=	p_Logo.Height - 72
st_Usuario.Width			=	This.Width - p_Logo.Width - 80
st_Computador.x			=	p_Logo.Width + 5
st_Computador.y			=	144
st_Computador.Height		=	p_Logo.Height - 144
st_Computador.Width		=	This.Width - p_Logo.Width - 80

end event

type pb_excel from w_para_informes`pb_excel within w_info_resumen_ordenproceso_kilos
integer x = 2688
integer y = 348
end type

type st_computador from w_para_informes`st_computador within w_info_resumen_ordenproceso_kilos
integer x = 1015
integer width = 3849
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumen_ordenproceso_kilos
integer x = 1015
integer width = 3849
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumen_ordenproceso_kilos
integer x = 1015
integer width = 3849
end type

type p_logo from w_para_informes`p_logo within w_info_resumen_ordenproceso_kilos
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumen_ordenproceso_kilos
integer width = 2290
string text = "Informe Resumen Ordenes Proceso"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumen_ordenproceso_kilos
integer x = 2688
integer y = 664
integer taborder = 170
integer weight = 400
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila, 	ll_Orden
Integer  	li_Variedad,  li_TipoInforme
Dec{2}   	ld_peso

If IsNull(ii_TipoOrden) Then
	MessageBox("Error de Datos", "Falta el Ingreso del Tipo de Proceso.")
	Return
End If

If ii_TipoOrden = 0 or cbx_todosproc.Checked Then
	ll_Orden = -1
Else
	ll_Orden		=	Long(em_Proceso.text)
	If IsNull(ll_Orden) Then
		MessageBox("Error de Datos", "Falta el Ingreso de un Número de Orden.")
		Return
	End If
End If

istr_info.Copias	=	1

OpenWithParm(vinf, istr_info)
Vinf.dw_1.DataObject =	"dw_info_resumenordenproceso"
Vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, ii_TipoOrden, ll_orden, uo_SelProductor.Codigo,&
									 uo_SelEspecie.Codigo, uo_SelVariedades.Codigo, Date(em_Desde.Text), Date(em_Hasta.Text))

If ll_Fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Object.DataWindow.Zoom = 58
	If gs_Ambiente <> 'Windows' Then  F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_resumen_ordenproceso_kilos
integer x = 2688
integer y = 928
integer taborder = 180
end type

type cb_buscaorden from commandbutton within w_info_resumen_ordenproceso_kilos
integer x = 2295
integer y = 956
integer width = 91
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;Str_busqueda	lstr_busq

CHOOSE CASE ii_TipoOrden
		
	CASE 4
		
		lstr_busq.argum[1]	=	String(uo_SelPlanta.Codigo)
		lstr_busq.argum[2]	=	"1"
		lstr_busq.argum[3]	=	string(ii_TipoOrden)
		lstr_busq.argum[4]	=	String(uo_SelCliente.Codigo)
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

type em_desde from editmask within w_info_resumen_ordenproceso_kilos
integer x = 759
integer y = 1068
integer width = 384
integer height = 84
integer taborder = 70
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_fecha from statictext within w_info_resumen_ordenproceso_kilos
integer x = 370
integer y = 1068
integer width = 347
integer height = 84
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_proceso from editmask within w_info_resumen_ordenproceso_kilos
integer x = 1893
integer y = 952
integer width = 384
integer height = 84
integer taborder = 60
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
	CASE 4, 5, 7, 8, 9
		IF NoExisteOrdenProceso(ll_orden) THEN
			This.Text=""
		END IF
		
END CHOOSE
end event

type st_7 from statictext within w_info_resumen_ordenproceso_kilos
integer x = 1495
integer y = 956
integer width = 347
integer height = 72
boolean bringtotop = true
integer textsize = -9
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

type st_6 from statictext within w_info_resumen_ordenproceso_kilos
integer x = 370
integer y = 700
integer width = 347
integer height = 68
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_resumen_ordenproceso_kilos
integer x = 370
integer y = 956
integer width = 347
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Tipo Proceso"
boolean focusrectangle = false
end type

type ddlb_tipoproc from dropdownlistbox within w_info_resumen_ordenproceso_kilos
integer x = 759
integer y = 944
integer width = 640
integer height = 368
integer taborder = 50
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
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
		em_Desde.Enabled			=	True
		em_Hasta.Enabled			=	True
		em_Hasta.Visible			=	True
		st_fecter.Visible			=	True
		cbx_todosproc.Checked	=	True
		cbx_todosproc.TriggerEvent(Clicked!)
		
	CASE 2
		ii_TipoOrden				=	index + 2
		em_proceso.Enabled		=	True
		em_proceso.Text			=	''
		cb_buscaorden.Enabled	=	True
		cbx_todosproc.Checked	=	False
		cbx_todosproc.Enabled	=	True
		em_Hasta.Visible			=	False
		st_fecter.Visible			=	False
		cbx_todosproc.TriggerEvent(Clicked!)
		
	CASE 3
		ii_TipoOrden				=	5
		em_proceso.Enabled		=	True
		em_proceso.Text			=	''
		cb_buscaorden.Enabled	=	True
		cbx_todosproc.Checked	=	False
		cbx_todosproc.Enabled	=	True
		em_Hasta.Visible			=	False
		st_fecter.Visible			=	False
		cbx_todosproc.TriggerEvent(Clicked!)
		
	CASE 4
		ii_TipoOrden				=	7
		em_proceso.Enabled		=	True
		em_proceso.Text			=	''
		cb_buscaorden.Enabled	=	True
		cbx_todosproc.Checked	=	False
		cbx_todosproc.Enabled	=	True
		em_Hasta.Visible			=	False
		st_fecter.Visible			=	False
		cbx_todosproc.TriggerEvent(Clicked!)
		
	CASE 5
		ii_TipoOrden				=	8
		em_proceso.Enabled		=	True
		em_proceso.Text			=	''
		cb_buscaorden.Enabled	=	True
		cbx_todosproc.Checked	=	False
		cbx_todosproc.Enabled	=	True
		em_Hasta.Visible			=	False
		st_fecter.Visible			=	False
		cbx_todosproc.TriggerEvent(Clicked!)				
END CHOOSE
end event

type st_1 from statictext within w_info_resumen_ordenproceso_kilos
integer x = 370
integer y = 1212
integer width = 347
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_resumen_ordenproceso_kilos
integer x = 370
integer y = 1432
integer width = 347
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_resumen_ordenproceso_kilos
integer x = 370
integer y = 1644
integer width = 347
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_resumen_ordenproceso_kilos
integer x = 1893
integer y = 1068
integer width = 384
integer height = 84
integer taborder = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_12 from statictext within w_info_resumen_ordenproceso_kilos
integer x = 370
integer y = 556
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

type st_fecter from statictext within w_info_resumen_ordenproceso_kilos
integer x = 1440
integer y = 1072
integer width = 402
integer height = 72
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Hasta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_resumen_ordenproceso_kilos
integer x = 251
integer y = 416
integer width = 2290
integer height = 416
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

type st_15 from statictext within w_info_resumen_ordenproceso_kilos
integer x = 251
integer y = 832
integer width = 2290
integer height = 940
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

type cbx_todosproc from checkbox within w_info_resumen_ordenproceso_kilos
integer x = 2135
integer y = 848
integer width = 251
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
end type

event clicked;If This.Checked Then
	em_proceso.Text					=	''
	cb_buscaorden.Enabled			=	False
	em_proceso.Enabled				=	False
	em_Desde.Enabled				=	True
	em_Hasta.Enabled					=	True
	uo_selProductor.Todos(True)
	uo_selVariedades.Todos(True)	
Else
	cb_buscaorden.Enabled			=	True
	em_proceso.Enabled				=	True
	em_Desde.Enabled				=	False
	em_Hasta.Enabled					=	False
End If
end event

type uo_selcliente from uo_seleccion_clientesprod within w_info_resumen_ordenproceso_kilos
event destroy ( )
integer x = 759
integer y = 464
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_info_resumen_ordenproceso_kilos
event destroy ( )
integer x = 759
integer y = 684
integer height = 96
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_info_resumen_ordenproceso_kilos
event destroy ( )
integer x = 759
integer y = 1200
integer height = 96
integer taborder = 130
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		uo_SelVariedades.Filtra(-1)
	Case Else
		uo_SelVariedades.Filtra(This.Codigo)
End Choose
end event

type uo_selproductor from uo_seleccion_productor within w_info_resumen_ordenproceso_kilos
event destroy ( )
integer x = 759
integer y = 1556
integer taborder = 140
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type uo_selvariedades from uo_seleccion_variedad within w_info_resumen_ordenproceso_kilos
event destroy ( )
integer x = 759
integer y = 1356
integer taborder = 140
boolean bringtotop = true
end type

on uo_selvariedades.destroy
call uo_seleccion_variedad::destroy
end on

