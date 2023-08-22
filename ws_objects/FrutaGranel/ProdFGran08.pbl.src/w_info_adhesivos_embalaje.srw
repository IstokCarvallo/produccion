$PBExportHeader$w_info_adhesivos_embalaje.srw
forward
global type w_info_adhesivos_embalaje from w_para_informes
end type
type dw_1 from datawindow within w_info_adhesivos_embalaje
end type
type st_1 from statictext within w_info_adhesivos_embalaje
end type
type st_2 from statictext within w_info_adhesivos_embalaje
end type
type st_3 from statictext within w_info_adhesivos_embalaje
end type
type em_emision from editmask within w_info_adhesivos_embalaje
end type
type uo_selembalaje from uo_seleccion_embalajeprod within w_info_adhesivos_embalaje
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_adhesivos_embalaje
end type
type dw_2 from datawindow within w_info_adhesivos_embalaje
end type
type gb_3 from groupbox within w_info_adhesivos_embalaje
end type
type gb_4 from groupbox within w_info_adhesivos_embalaje
end type
end forward

global type w_info_adhesivos_embalaje from w_para_informes
integer x = 14
integer y = 32
integer width = 2194
integer height = 1260
string title = "IMPRESION DE ADHESIVOS CAJAS"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
st_1 st_1
st_2 st_2
st_3 st_3
em_emision em_emision
uo_selembalaje uo_selembalaje
uo_selcliente uo_selcliente
dw_2 dw_2
gb_3 gb_3
gb_4 gb_4
end type
global w_info_adhesivos_embalaje w_info_adhesivos_embalaje

type variables

end variables

on w_info_adhesivos_embalaje.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.em_emision=create em_emision
this.uo_selembalaje=create uo_selembalaje
this.uo_selcliente=create uo_selcliente
this.dw_2=create dw_2
this.gb_3=create gb_3
this.gb_4=create gb_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.em_emision
this.Control[iCurrent+6]=this.uo_selembalaje
this.Control[iCurrent+7]=this.uo_selcliente
this.Control[iCurrent+8]=this.dw_2
this.Control[iCurrent+9]=this.gb_3
this.Control[iCurrent+10]=this.gb_4
end on

on w_info_adhesivos_embalaje.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.em_emision)
destroy(this.uo_selembalaje)
destroy(this.uo_selcliente)
destroy(this.dw_2)
destroy(this.gb_3)
destroy(this.gb_4)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelCliente.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelEmbalaje.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelCliente.Seleccion(False,False)
	uo_SelEmbalaje.Seleccion(False,False)
	uo_SelEmbalaje.Filtra(gi_CodExport)
	uo_SelCliente.dw_Seleccion.Object.codigo[1]	= gi_CodExport
	
	dw_1.SetTransObject(Sqlca)
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_adhesivos_embalaje
end type

type st_computador from w_para_informes`st_computador within w_info_adhesivos_embalaje
end type

type st_usuario from w_para_informes`st_usuario within w_info_adhesivos_embalaje
end type

type st_temporada from w_para_informes`st_temporada within w_info_adhesivos_embalaje
end type

type p_logo from w_para_informes`p_logo within w_info_adhesivos_embalaje
end type

type st_titulo from w_para_informes`st_titulo within w_info_adhesivos_embalaje
integer width = 1454
string text = "Impresión de Adhesivos Embalajes de Cajas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_adhesivos_embalaje
string tag = "Imprimir Reporte"
integer x = 1810
integer y = 484
integer taborder = 100
end type

event pb_acepta::clicked;Long		ll_fila_e, respuesta, ll_fila, ll_actual, ll_control, ll_controlimp

IF MOD(Integer(em_emision.Text), 4) <> 0 THEN
	MessageBox("Error de protección de datos", "Solo debe imprimir adhesivos en multiplos de 4")
	Return
END IF

Do
	ll_fila_e	=	dw_1.Retrieve(uo_SelEmbalaje.Codigo, Integer(em_emision.Text))
										  
	If ll_fila_e = -1 Then
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)		
	ElseIf ll_fila_e < 1 Then 
		SetPointer(Arrow!)
		Return
	Else
		FOR ll_fila = 1 TO dw_1.RowCount()
			ll_actual							=	dw_2.InsertRow(0)			
			ll_control ++
			
			CHOOSE CASE ll_control
				CASE 1
					dw_2.Object.Ole_1.Object.Text	=	dw_1.Object.codigo[ll_fila]
					
				CASE 2
					dw_2.Object.Ole_2.Object.Text	=	dw_1.Object.codigo[ll_fila]
					
				CASE 3
					dw_2.Object.Ole_3.Object.Text	=	dw_1.Object.codigo[ll_fila]
					
				CASE 4
					dw_2.Object.Ole_4.Object.Text	=	dw_1.Object.codigo[ll_fila]
					ll_controlimp						=	dw_2.Print()
					
					ll_control							=	0
					dw_2.RowsMove(1, dw_2.RowCount(), Primary!, dw_2, 1, Delete!)
									
			END CHOOSE
			
			IF ll_controlimp = -1 THEN
				EXIT
			END IF
		NEXT
		
		IF ll_controlimp = -1 THEN 
			MessageBox("Error", "No se pudo realizar la impresión")
			SetPointer(Arrow!)
			RETURN
		END IF
	END IF
LOOP WHILE respuesta = 1

Disconnect Using Sqlca;
Connect Using Sqlca;
dw_1.SetTransObject(Sqlca)

IF respuesta = 2 Then Close(Parent)


end event

type pb_salir from w_para_informes`pb_salir within w_info_adhesivos_embalaje
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1806
integer y = 744
integer taborder = 110
string disabledname = "\desarrollo\bmp\exitd.bmp"
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type dw_1 from datawindow within w_info_adhesivos_embalaje
boolean visible = false
integer x = 1266
integer y = 988
integer width = 558
integer height = 376
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_genera_adhesivo_embalaje"
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_info_adhesivos_embalaje
integer x = 320
integer y = 500
integer width = 233
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
string text = "Cliente"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_adhesivos_embalaje
integer x = 315
integer y = 668
integer width = 288
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
string text = "Embalaje"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_adhesivos_embalaje
integer x = 325
integer y = 972
integer width = 549
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
string text = "Adhesivos a Emitir"
boolean focusrectangle = false
end type

type em_emision from editmask within w_info_adhesivos_embalaje
integer x = 946
integer y = 960
integer width = 402
integer height = 92
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "###"
end type

type uo_selembalaje from uo_seleccion_embalajeprod within w_info_adhesivos_embalaje
event destroy ( )
integer x = 704
integer y = 656
integer height = 88
integer taborder = 110
boolean bringtotop = true
end type

on uo_selembalaje.destroy
call uo_seleccion_embalajeprod::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_info_adhesivos_embalaje
event destroy ( )
integer x = 704
integer y = 488
integer height = 84
integer taborder = 110
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.codigo)  Then Return

Choose Case This.codigo
	Case -1, -9
		
	Case This.codigo
		uo_selembalaje.Filtra(This.codigo)
		uo_SelEmbalaje.LimpiaDatos()
		
End Choose
end event

type dw_2 from datawindow within w_info_adhesivos_embalaje
boolean visible = false
integer x = 215
integer y = 1048
integer width = 686
integer height = 400
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_adhesivos_embaladorax4"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type gb_3 from groupbox within w_info_adhesivos_embalaje
integer x = 251
integer y = 424
integer width = 1449
integer height = 372
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
end type

type gb_4 from groupbox within w_info_adhesivos_embalaje
integer x = 251
integer y = 812
integer width = 1449
integer height = 324
integer taborder = 130
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
end type

