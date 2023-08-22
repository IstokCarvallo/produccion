$PBExportHeader$w_info_guiasnulas.srw
$PBExportComments$Informe de Existencia de Fruta Comercial en Camara por Productor y Calibre.
forward
global type w_info_guiasnulas from w_para_informes
end type
type st_1 from statictext within w_info_guiasnulas
end type
type st_3 from statictext within w_info_guiasnulas
end type
type uo_selplanta from uo_seleccion_plantas within w_info_guiasnulas
end type
type st_6 from statictext within w_info_guiasnulas
end type
type st_11 from statictext within w_info_guiasnulas
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_guiasnulas
end type
type st_9 from statictext within w_info_guiasnulas
end type
type st_10 from statictext within w_info_guiasnulas
end type
type em_desde from editmask within w_info_guiasnulas
end type
type em_hasta from editmask within w_info_guiasnulas
end type
type ddlb_modulo from dropdownlistbox within w_info_guiasnulas
end type
type st_2 from statictext within w_info_guiasnulas
end type
type cbx_modulo from checkbox within w_info_guiasnulas
end type
end forward

global type w_info_guiasnulas from w_para_informes
integer x = 0
integer y = 0
integer width = 2304
integer height = 1800
string title = "INFORME DE TRASVASIJE"
st_1 st_1
st_3 st_3
uo_selplanta uo_selplanta
st_6 st_6
st_11 st_11
uo_selcliente uo_selcliente
st_9 st_9
st_10 st_10
em_desde em_desde
em_hasta em_hasta
ddlb_modulo ddlb_modulo
st_2 st_2
cbx_modulo cbx_modulo
end type
global w_info_guiasnulas w_info_guiasnulas

type variables
Integer	ib_Modulo = -1
end variables

forward prototypes
public subroutine consolidalote ()
end prototypes

public subroutine consolidalote ();
end subroutine

on w_info_guiasnulas.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_3=create st_3
this.uo_selplanta=create uo_selplanta
this.st_6=create st_6
this.st_11=create st_11
this.uo_selcliente=create uo_selcliente
this.st_9=create st_9
this.st_10=create st_10
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.ddlb_modulo=create ddlb_modulo
this.st_2=create st_2
this.cbx_modulo=create cbx_modulo
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.uo_selplanta
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.st_11
this.Control[iCurrent+6]=this.uo_selcliente
this.Control[iCurrent+7]=this.st_9
this.Control[iCurrent+8]=this.st_10
this.Control[iCurrent+9]=this.em_desde
this.Control[iCurrent+10]=this.em_hasta
this.Control[iCurrent+11]=this.ddlb_modulo
this.Control[iCurrent+12]=this.st_2
this.Control[iCurrent+13]=this.cbx_modulo
end on

on w_info_guiasnulas.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_3)
destroy(this.uo_selplanta)
destroy(this.st_6)
destroy(this.st_11)
destroy(this.uo_selcliente)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.ddlb_modulo)
destroy(this.st_2)
destroy(this.cbx_modulo)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlanta.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelCliente.Seleccion(True, False)
	uo_SelPlanta.Seleccion(True,False)

	em_desde.Text	= String(Today(), 'dd/mm/yyyy')
	em_hasta.Text	= String(Today(), 'dd/mm/yyyy')
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_guiasnulas
integer x = 1819
integer y = 408
end type

type st_computador from w_para_informes`st_computador within w_info_guiasnulas
integer x = 1006
integer width = 2405
end type

type st_usuario from w_para_informes`st_usuario within w_info_guiasnulas
integer x = 1006
integer width = 2405
end type

type st_temporada from w_para_informes`st_temporada within w_info_guiasnulas
integer x = 1006
integer width = 2405
end type

type p_logo from w_para_informes`p_logo within w_info_guiasnulas
end type

type st_titulo from w_para_informes`st_titulo within w_info_guiasnulas
integer width = 1394
string text = "Informe Guias Nulas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_guiasnulas
integer x = 1819
integer y = 748
integer taborder = 120
integer textsize = -8
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila

istr_info.titulo	= "INFORME GUIAS DESPACHO NULAS"
istr_info.copias	= 1
	
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_GuiasNulas"
vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, DateTime(em_Desde.Text), DateTime(em_Hasta.Text), ib_Modulo)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Object.DataWindow.Zoom = 95
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_guiasnulas
integer x = 1824
integer y = 1048
integer taborder = 140
integer textsize = -8
fontcharset fontcharset = ansi!
end type

type st_1 from statictext within w_info_guiasnulas
integer x = 251
integer y = 416
integer width = 1394
integer height = 456
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_guiasnulas
integer x = 315
integer y = 544
integer width = 338
integer height = 64
boolean bringtotop = true
integer textsize = -8
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

type uo_selplanta from uo_seleccion_plantas within w_info_guiasnulas
integer x = 649
integer y = 652
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
	//	uo_SelCama.Todos(True)
		
		//uo_SelCama.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		//uo_SelCama.Filtra(This.Codigo)
		
		//uo_SelCama.cbx_Todos.Enabled	=	True
		
END CHOOSE

ConsolidaLote()
end event

type st_6 from statictext within w_info_guiasnulas
integer x = 329
integer y = 748
integer width = 274
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_guiasnulas
integer x = 251
integer y = 876
integer width = 1394
integer height = 500
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_info_guiasnulas
event destroy ( )
integer x = 649
integer y = 444
integer taborder = 50
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type st_9 from statictext within w_info_guiasnulas
integer x = 302
integer y = 940
integer width = 219
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Desde"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_guiasnulas
integer x = 951
integer y = 940
integer width = 201
integer height = 64
boolean bringtotop = true
integer textsize = -8
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

type em_desde from editmask within w_info_guiasnulas
integer x = 480
integer y = 928
integer width = 443
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type em_hasta from editmask within w_info_guiasnulas
integer x = 1143
integer y = 928
integer width = 443
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type ddlb_modulo from dropdownlistbox within w_info_guiasnulas
integer x = 576
integer y = 1140
integer width = 1010
integer height = 324
integer taborder = 150
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
boolean sorted = false
string item[] = {"Fruta Embalada","Envases","Fruta Comercial","Fruta Granel"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ib_Modulo = Index
end event

type st_2 from statictext within w_info_guiasnulas
integer x = 302
integer y = 1152
integer width = 219
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Modulo"
boolean focusrectangle = false
end type

type cbx_modulo from checkbox within w_info_guiasnulas
integer x = 576
integer y = 1052
integer width = 402
integer height = 64
boolean bringtotop = true
integer textsize = -8
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

event clicked;If This.Checked Then
	ib_Modulo	=	-1
	ddlb_modulo.Enabled = False
Else
	ib_Modulo	= 0
	ddlb_modulo.Enabled = True
End If
end event

