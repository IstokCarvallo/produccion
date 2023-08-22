$PBExportHeader$w_info_existenciamultimercado.srw
$PBExportComments$MantenciónAnulación de Inspección por Vencimiento.
forward
global type w_info_existenciamultimercado from w_para_informes
end type
type st_4 from statictext within w_info_existenciamultimercado
end type
type st_1 from statictext within w_info_existenciamultimercado
end type
type st_6 from statictext within w_info_existenciamultimercado
end type
type uo_selplanta from uo_seleccion_plantas within w_info_existenciamultimercado
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_existenciamultimercado
end type
end forward

global type w_info_existenciamultimercado from w_para_informes
integer x = 14
integer y = 32
integer width = 2747
integer height = 1376
string title = "Existencia Multimercado"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_6 st_6
uo_selplanta uo_selplanta
uo_selcliente uo_selcliente
end type
global w_info_existenciamultimercado w_info_existenciamultimercado

type variables

end variables

on w_info_existenciamultimercado.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_6=create st_6
this.uo_selplanta=create uo_selplanta
this.uo_selcliente=create uo_selcliente
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_6
this.Control[iCurrent+4]=this.uo_selplanta
this.Control[iCurrent+5]=this.uo_selcliente
end on

on w_info_existenciamultimercado.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_6)
destroy(this.uo_selplanta)
destroy(this.uo_selcliente)
end on

event open;call super::open;Boolean lb_Cerrar

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(True,False)
	uo_SelPlanta.Seleccion(True, False)
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_existenciamultimercado
integer x = 2363
integer y = 400
end type

type st_computador from w_para_informes`st_computador within w_info_existenciamultimercado
end type

type st_usuario from w_para_informes`st_usuario within w_info_existenciamultimercado
end type

type st_temporada from w_para_informes`st_temporada within w_info_existenciamultimercado
end type

type p_logo from w_para_informes`p_logo within w_info_existenciamultimercado
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_existenciamultimercado
integer width = 1957
string text = "Existencia Multimercado"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existenciamultimercado
string tag = "Imprimir Reporte"
integer x = 2377
integer y = 684
integer taborder = 100
end type

event pb_acepta::clicked;SetPointer(Arrow!)
Long		ll_Fila

istr_info.titulo	= 'INFORME EXISTENCIA MULTIMERCADO'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_existenciamultimercado"
vinf.dw_1.SetTransObject(SQLCA)
ll_fila	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo)
								 
IF ll_fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Zoom = 92')
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_existenciamultimercado
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2373
integer y = 996
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_existenciamultimercado
integer x = 251
integer y = 440
integer width = 1961
integer height = 652
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_existenciamultimercado
integer x = 334
integer y = 848
integer width = 462
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

type st_6 from statictext within w_info_existenciamultimercado
integer x = 352
integer y = 604
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

type uo_selplanta from uo_seleccion_plantas within w_info_existenciamultimercado
integer x = 782
integer y = 760
integer taborder = 110
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_info_existenciamultimercado
integer x = 782
integer y = 496
integer taborder = 20
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

