$PBExportHeader$w_info_resumenrecepciones.srw
$PBExportComments$Informe de Existencia de Fruta Comercial en Camara por Productor y Calibre.
forward
global type w_info_resumenrecepciones from w_para_informes
end type
type st_1 from statictext within w_info_resumenrecepciones
end type
type st_3 from statictext within w_info_resumenrecepciones
end type
type st_7 from statictext within w_info_resumenrecepciones
end type
type st_9 from statictext within w_info_resumenrecepciones
end type
type st_10 from statictext within w_info_resumenrecepciones
end type
type em_desde from editmask within w_info_resumenrecepciones
end type
type em_hasta from editmask within w_info_resumenrecepciones
end type
type uo_selcontratista from uo_seleccion_contratista within w_info_resumenrecepciones
end type
end forward

global type w_info_resumenrecepciones from w_para_informes
integer x = 0
integer y = 0
integer width = 2373
integer height = 1360
string title = "INFORME DE TRASVASIJE"
st_1 st_1
st_3 st_3
st_7 st_7
st_9 st_9
st_10 st_10
em_desde em_desde
em_hasta em_hasta
uo_selcontratista uo_selcontratista
end type
global w_info_resumenrecepciones w_info_resumenrecepciones

type variables

end variables

forward prototypes
public subroutine consolidalote ()
end prototypes

public subroutine consolidalote ();
end subroutine

on w_info_resumenrecepciones.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_3=create st_3
this.st_7=create st_7
this.st_9=create st_9
this.st_10=create st_10
this.em_desde=create em_desde
this.em_hasta=create em_hasta
this.uo_selcontratista=create uo_selcontratista
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_7
this.Control[iCurrent+4]=this.st_9
this.Control[iCurrent+5]=this.st_10
this.Control[iCurrent+6]=this.em_desde
this.Control[iCurrent+7]=this.em_hasta
this.Control[iCurrent+8]=this.uo_selcontratista
end on

on w_info_resumenrecepciones.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.em_desde)
destroy(this.em_hasta)
destroy(this.uo_selcontratista)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelContratista.Codigo) Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelContratista.Seleccion(True, False)
	
	em_desde.Text	= String(Today(), 'dd/mm/yyyy')
	em_hasta.Text	= String(Today(), 'dd/mm/yyyy')
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_resumenrecepciones
integer x = 1883
integer y = 280
end type

type st_computador from w_para_informes`st_computador within w_info_resumenrecepciones
integer x = 1006
integer width = 1257
end type

type st_usuario from w_para_informes`st_usuario within w_info_resumenrecepciones
integer x = 1006
integer width = 1257
end type

type st_temporada from w_para_informes`st_temporada within w_info_resumenrecepciones
integer x = 1006
integer width = 1257
end type

type p_logo from w_para_informes`p_logo within w_info_resumenrecepciones
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_resumenrecepciones
integer width = 1499
string text = "Informe Resumen Recepciones"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resumenrecepciones
integer x = 1883
integer y = 620
integer taborder = 120
integer textsize = -8
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila

istr_info.titulo	= "INFORME RESUMEN RECEPCIONES"
istr_info.copias	= 1
	
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_resumenrecepcion"
vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(uo_SelContratista.Codigo, DateTime(em_Desde.Text), DateTime(em_Hasta.Text))

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
//	vinf.dw_1.Object.DataWindow.Zoom = 95
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_resumenrecepciones
integer x = 1888
integer y = 920
integer taborder = 140
integer textsize = -8
fontcharset fontcharset = ansi!
end type

type st_1 from statictext within w_info_resumenrecepciones
integer x = 251
integer y = 416
integer width = 1499
integer height = 364
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

type st_3 from statictext within w_info_resumenrecepciones
integer x = 311
integer y = 612
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
string text = "Contratista"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_resumenrecepciones
integer x = 251
integer y = 784
integer width = 1499
integer height = 224
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

type st_9 from statictext within w_info_resumenrecepciones
integer x = 347
integer y = 872
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

type st_10 from statictext within w_info_resumenrecepciones
integer x = 997
integer y = 872
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

type em_desde from editmask within w_info_resumenrecepciones
integer x = 526
integer y = 860
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

type em_hasta from editmask within w_info_resumenrecepciones
integer x = 1189
integer y = 856
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

type uo_selcontratista from uo_seleccion_contratista within w_info_resumenrecepciones
event destroy ( )
integer x = 695
integer y = 524
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcontratista.destroy
call uo_seleccion_contratista::destroy
end on

