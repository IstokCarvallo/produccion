$PBExportHeader$w_info_lotes_vaciados_pp.srw
forward
global type w_info_lotes_vaciados_pp from w_para_informes
end type
type gb_3 from groupbox within w_info_lotes_vaciados_pp
end type
type st_6 from statictext within w_info_lotes_vaciados_pp
end type
type st_1 from statictext within w_info_lotes_vaciados_pp
end type
type st_8 from statictext within w_info_lotes_vaciados_pp
end type
type st_3 from statictext within w_info_lotes_vaciados_pp
end type
type st_variedad from statictext within w_info_lotes_vaciados_pp
end type
type st_5 from statictext within w_info_lotes_vaciados_pp
end type
type st_4 from statictext within w_info_lotes_vaciados_pp
end type
type uo_selespecie from uo_seleccion_especie within w_info_lotes_vaciados_pp
end type
type uo_selplanta from uo_seleccion_plantas within w_info_lotes_vaciados_pp
end type
type uo_selproductor from uo_seleccion_productor within w_info_lotes_vaciados_pp
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_lotes_vaciados_pp
end type
type st_2 from statictext within w_info_lotes_vaciados_pp
end type
type em_desde from editmask within w_info_lotes_vaciados_pp
end type
type st_7 from statictext within w_info_lotes_vaciados_pp
end type
type em_hasta from editmask within w_info_lotes_vaciados_pp
end type
type em_orden from editmask within w_info_lotes_vaciados_pp
end type
type cbx_ordentodo from checkbox within w_info_lotes_vaciados_pp
end type
type st_17 from statictext within w_info_lotes_vaciados_pp
end type
type cbx_todosfecha from checkbox within w_info_lotes_vaciados_pp
end type
type r_1 from rectangle within w_info_lotes_vaciados_pp
end type
type gb_4 from groupbox within w_info_lotes_vaciados_pp
end type
type st_10 from statictext within w_info_lotes_vaciados_pp
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_lotes_vaciados_pp
end type
type ddlb_orden from dropdownlistbox within w_info_lotes_vaciados_pp
end type
type st_9 from statictext within w_info_lotes_vaciados_pp
end type
type dw_1 from datawindow within w_info_lotes_vaciados_pp
end type
end forward

global type w_info_lotes_vaciados_pp from w_para_informes
integer width = 3995
integer height = 1776
boolean minbox = false
boolean maxbox = false
gb_3 gb_3
st_6 st_6
st_1 st_1
st_8 st_8
st_3 st_3
st_variedad st_variedad
st_5 st_5
st_4 st_4
uo_selespecie uo_selespecie
uo_selplanta uo_selplanta
uo_selproductor uo_selproductor
uo_selvariedad uo_selvariedad
st_2 st_2
em_desde em_desde
st_7 st_7
em_hasta em_hasta
em_orden em_orden
cbx_ordentodo cbx_ordentodo
st_17 st_17
cbx_todosfecha cbx_todosfecha
r_1 r_1
gb_4 gb_4
st_10 st_10
uo_selcliente uo_selcliente
ddlb_orden ddlb_orden
st_9 st_9
dw_1 dw_1
end type
global w_info_lotes_vaciados_pp w_info_lotes_vaciados_pp

type variables
Integer	ii_nroorden, ii_tipoorden
end variables

forward prototypes
public function boolean noexisteetiqueta (integer li_etiqueta)
end prototypes

public function boolean noexisteetiqueta (integer li_etiqueta);String	ls_nombre


SELECT	etiq_nombre
INTO    :ls_nombre
FROM	dba.etiquetas
WHERE	etiq_codigo =  :li_etiqueta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Etiquetas")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Etiqueta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	RETURN False 
END IF
end function

on w_info_lotes_vaciados_pp.create
int iCurrent
call super::create
this.gb_3=create gb_3
this.st_6=create st_6
this.st_1=create st_1
this.st_8=create st_8
this.st_3=create st_3
this.st_variedad=create st_variedad
this.st_5=create st_5
this.st_4=create st_4
this.uo_selespecie=create uo_selespecie
this.uo_selplanta=create uo_selplanta
this.uo_selproductor=create uo_selproductor
this.uo_selvariedad=create uo_selvariedad
this.st_2=create st_2
this.em_desde=create em_desde
this.st_7=create st_7
this.em_hasta=create em_hasta
this.em_orden=create em_orden
this.cbx_ordentodo=create cbx_ordentodo
this.st_17=create st_17
this.cbx_todosfecha=create cbx_todosfecha
this.r_1=create r_1
this.gb_4=create gb_4
this.st_10=create st_10
this.uo_selcliente=create uo_selcliente
this.ddlb_orden=create ddlb_orden
this.st_9=create st_9
this.dw_1=create dw_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_3
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_8
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_variedad
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.st_4
this.Control[iCurrent+9]=this.uo_selespecie
this.Control[iCurrent+10]=this.uo_selplanta
this.Control[iCurrent+11]=this.uo_selproductor
this.Control[iCurrent+12]=this.uo_selvariedad
this.Control[iCurrent+13]=this.st_2
this.Control[iCurrent+14]=this.em_desde
this.Control[iCurrent+15]=this.st_7
this.Control[iCurrent+16]=this.em_hasta
this.Control[iCurrent+17]=this.em_orden
this.Control[iCurrent+18]=this.cbx_ordentodo
this.Control[iCurrent+19]=this.st_17
this.Control[iCurrent+20]=this.cbx_todosfecha
this.Control[iCurrent+21]=this.r_1
this.Control[iCurrent+22]=this.gb_4
this.Control[iCurrent+23]=this.st_10
this.Control[iCurrent+24]=this.uo_selcliente
this.Control[iCurrent+25]=this.ddlb_orden
this.Control[iCurrent+26]=this.st_9
this.Control[iCurrent+27]=this.dw_1
end on

on w_info_lotes_vaciados_pp.destroy
call super::destroy
destroy(this.gb_3)
destroy(this.st_6)
destroy(this.st_1)
destroy(this.st_8)
destroy(this.st_3)
destroy(this.st_variedad)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.uo_selespecie)
destroy(this.uo_selplanta)
destroy(this.uo_selproductor)
destroy(this.uo_selvariedad)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.em_orden)
destroy(this.cbx_ordentodo)
destroy(this.st_17)
destroy(this.cbx_todosfecha)
destroy(this.r_1)
destroy(this.gb_4)
destroy(this.st_10)
destroy(this.uo_selcliente)
destroy(this.ddlb_orden)
destroy(this.st_9)
destroy(this.dw_1)
end on

event open;call super::open;Boolean lb_cerrar

IF IsNull(uo_SelCliente.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelProductor.Codigo)THEN lb_Cerrar	=	True
IF IsNull(uo_SelEspecie.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(uo_SelVariedad.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
END IF

uo_SelPlanta.Seleccion(True, False)
uo_SelEspecie.Seleccion(True, False)
uo_SelCliente.Seleccion(True, False)
uo_SelVariedad.Seleccion(True, False)
uo_SelProductor.Seleccion(True, False)
uo_SelProductor.Filtra(-1)

cbx_todosfecha.TriggerEvent("clicked")

em_desde.Text 	= 	String(RelativeDate(Today(), -365))
em_hasta.Text 		= 	String(Today())

ddlb_orden.SelectItem(1)
ii_TipoOrden		=	-1
end event

type pb_excel from w_para_informes`pb_excel within w_info_lotes_vaciados_pp
integer x = 3593
integer y = 632
integer taborder = 0
end type

type st_computador from w_para_informes`st_computador within w_info_lotes_vaciados_pp
end type

type st_usuario from w_para_informes`st_usuario within w_info_lotes_vaciados_pp
end type

type st_temporada from w_para_informes`st_temporada within w_info_lotes_vaciados_pp
end type

type p_logo from w_para_informes`p_logo within w_info_lotes_vaciados_pp
end type

type st_titulo from w_para_informes`st_titulo within w_info_lotes_vaciados_pp
integer width = 3259
string text = "Informe de Lotes Vaciados"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_lotes_vaciados_pp
integer x = 3639
integer y = 960
integer taborder = 100
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;Integer		li_consfecha
String			ls_Archivo,	ls_ruta
Long			ll_nroorden, 	fila
Date			ld_desde, ld_hasta

SetPointer(HourGlass!)

istr_info.titulo	= "INFORME DE LOTES VACIADOS"
istr_info.copias	= 1

IF cbx_ordentodo.Checked THEN
	ll_nroOrden	= 	-1
	ll_nroOrden	= 	-9
ELSE
	ll_nroOrden 	= 	Long(em_orden.Text)
END IF

li_ConsFecha = 1
ld_desde			=	Date(em_desde.Text)
ld_hasta			=	Date(em_hasta.Text)

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_informe_lotes_vaciados_pp"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo,		uo_Selplanta.Codigo, 	ii_tipoorden,&
								  ll_nroorden,					uo_SelProductor.Codigo,	uo_SelEspecie.Codigo,&
								  uo_SelVariedad.Codigo,	ld_desde,ld_hasta)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify("Cliente.text = '" + uo_SelCliente.Nombre + "'")
	vinf.dw_1.Object.DataWindow.Zoom = 98
	
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	vinf.Visible			= 	True
	vinf.Enabled			= 	True
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_lotes_vaciados_pp
integer x = 3630
integer y = 1252
integer taborder = 110
fontcharset fontcharset = ansi!
end type

type gb_3 from groupbox within w_info_lotes_vaciados_pp
integer x = 379
integer y = 1304
integer width = 3031
integer height = 196
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554431
long backcolor = 16711680
end type

type st_6 from statictext within w_info_lotes_vaciados_pp
integer x = 297
integer y = 540
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

type st_1 from statictext within w_info_lotes_vaciados_pp
integer x = 297
integer y = 728
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

type st_8 from statictext within w_info_lotes_vaciados_pp
integer x = 297
integer y = 944
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

type st_3 from statictext within w_info_lotes_vaciados_pp
integer x = 1888
integer y = 732
integer width = 270
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
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_lotes_vaciados_pp
integer x = 1879
integer y = 928
integer width = 302
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_lotes_vaciados_pp
integer x = 1787
integer y = 424
integer width = 1714
integer height = 628
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554431
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_lotes_vaciados_pp
integer x = 242
integer y = 424
integer width = 1545
integer height = 628
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554431
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_lotes_vaciados_pp
event destroy ( )
integer x = 2226
integer y = 640
integer taborder = 40
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_selvariedad.cbx_Todos.Checked			=	True
		uo_selvariedad.cbx_Consolida.Enabled		=	True
		uo_selvariedad.dw_Seleccion.Enabled		=	False		
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)
END CHOOSE
end event

type uo_selplanta from uo_seleccion_plantas within w_info_lotes_vaciados_pp
event destroy ( )
integer x = 805
integer y = 640
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selproductor from uo_seleccion_productor within w_info_lotes_vaciados_pp
event destroy ( )
integer x = 805
integer y = 840
integer taborder = 30
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type uo_selvariedad from uo_seleccion_variedad within w_info_lotes_vaciados_pp
event destroy ( )
integer x = 2240
integer y = 840
integer taborder = 50
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type st_2 from statictext within w_info_lotes_vaciados_pp
integer x = 448
integer y = 1388
integer width = 425
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
string text = "Fecha Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_lotes_vaciados_pp
integer x = 919
integer y = 1372
integer width = 439
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_7 from statictext within w_info_lotes_vaciados_pp
integer x = 1394
integer y = 1388
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

type em_hasta from editmask within w_info_lotes_vaciados_pp
integer x = 1714
integer y = 1372
integer width = 439
integer height = 96
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type em_orden from editmask within w_info_lotes_vaciados_pp
integer x = 919
integer y = 1156
integer width = 439
integer height = 96
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
string minmax = "~~8"
end type

event modified;ii_nroorden	=	Long(This.Text)
end event

type cbx_ordentodo from checkbox within w_info_lotes_vaciados_pp
integer x = 2226
integer y = 1168
integer width = 293
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
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_orden.Enabled										=	False
	em_orden.Text 											=	''
	ii_nroorden													=	-1
ELSE
	em_orden.Enabled										=	True
	ii_nroorden													=	Long(em_orden.Text)
	em_orden.SetFocus()
END IF
end event

type st_17 from statictext within w_info_lotes_vaciados_pp
integer x = 448
integer y = 1172
integer width = 443
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
string text = "Orden Proceso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_todosfecha from checkbox within w_info_lotes_vaciados_pp
integer x = 2226
integer y = 1384
integer width = 293
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
string text = "Todas"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_desde.Text 		= String(RelativeDate(Today(), -365))
	em_hasta.Text 		= String(Today())
	em_desde.enabled	= False
	em_hasta.enabled		= False
ELSE
	em_desde.enabled	= True
	em_hasta.enabled		= True
END IF
end event

type r_1 from rectangle within w_info_lotes_vaciados_pp
integer linethickness = 4
long fillcolor = 16777215
integer x = 2587
integer y = 1208
integer width = 229
integer height = 200
end type

type gb_4 from groupbox within w_info_lotes_vaciados_pp
integer x = 379
integer y = 1088
integer width = 3031
integer height = 196
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
end type

type st_10 from statictext within w_info_lotes_vaciados_pp
integer x = 242
integer y = 1056
integer width = 3259
integer height = 516
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_lotes_vaciados_pp
integer x = 805
integer y = 456
integer taborder = 10
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type ddlb_orden from dropdownlistbox within w_info_lotes_vaciados_pp
integer x = 1714
integer y = 1156
integer width = 480
integer height = 328
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
boolean sorted = false
boolean vscrollbar = true
string item[] = {"Todos","Proceso","ReProceso","PreProceso"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE index
	CASE 1
		ii_tipoorden	=	-1
		
	CASE 2
		ii_tipoorden	=	4
		
	CASE 3
		ii_tipoorden	=	5
		
	CASE 4
		ii_tipoorden	=	8
		
END CHOOSE
end event

type st_9 from statictext within w_info_lotes_vaciados_pp
integer x = 1394
integer y = 1172
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
string text = "Tipo"
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_info_lotes_vaciados_pp
boolean visible = false
integer x = 3598
integer y = 276
integer width = 151
integer height = 132
boolean bringtotop = true
string title = "none"
string dataobject = "dw_informe_lotes_vaciados_pp"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

