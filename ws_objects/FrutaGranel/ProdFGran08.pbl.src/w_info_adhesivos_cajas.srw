$PBExportHeader$w_info_adhesivos_cajas.srw
forward
global type w_info_adhesivos_cajas from w_para_informes
end type
type dw_1 from datawindow within w_info_adhesivos_cajas
end type
type uo_sellinea from uo_seleccion_lineapacking within w_info_adhesivos_cajas
end type
type st_1 from statictext within w_info_adhesivos_cajas
end type
type st_2 from statictext within w_info_adhesivos_cajas
end type
type st_3 from statictext within w_info_adhesivos_cajas
end type
type em_emision from editmask within w_info_adhesivos_cajas
end type
type uo_selsalida from uo_seleccion_salidapacking within w_info_adhesivos_cajas
end type
type st_4 from statictext within w_info_adhesivos_cajas
end type
type em_actual from editmask within w_info_adhesivos_cajas
end type
type st_5 from statictext within w_info_adhesivos_cajas
end type
type ddlb_lado from dropdownlistbox within w_info_adhesivos_cajas
end type
type dw_2 from datawindow within w_info_adhesivos_cajas
end type
type gb_3 from groupbox within w_info_adhesivos_cajas
end type
type gb_4 from groupbox within w_info_adhesivos_cajas
end type
type ole_1 from olecustomcontrol within w_info_adhesivos_cajas
end type
type cbx_x5 from checkbox within w_info_adhesivos_cajas
end type
end forward

global type w_info_adhesivos_cajas from w_para_informes
integer x = 14
integer y = 32
integer width = 2130
integer height = 1396
string title = "IMPRESION DE ADHESIVOS CAJAS"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
uo_sellinea uo_sellinea
st_1 st_1
st_2 st_2
st_3 st_3
em_emision em_emision
uo_selsalida uo_selsalida
st_4 st_4
em_actual em_actual
st_5 st_5
ddlb_lado ddlb_lado
dw_2 dw_2
gb_3 gb_3
gb_4 gb_4
ole_1 ole_1
cbx_x5 cbx_x5
end type
global w_info_adhesivos_cajas w_info_adhesivos_cajas

type variables
str_mant					istr_mant
uo_correladhesivos	iuo_adhesivos
String					is_Lado
end variables

on w_info_adhesivos_cajas.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.uo_sellinea=create uo_sellinea
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.em_emision=create em_emision
this.uo_selsalida=create uo_selsalida
this.st_4=create st_4
this.em_actual=create em_actual
this.st_5=create st_5
this.ddlb_lado=create ddlb_lado
this.dw_2=create dw_2
this.gb_3=create gb_3
this.gb_4=create gb_4
this.ole_1=create ole_1
this.cbx_x5=create cbx_x5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.uo_sellinea
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.em_emision
this.Control[iCurrent+7]=this.uo_selsalida
this.Control[iCurrent+8]=this.st_4
this.Control[iCurrent+9]=this.em_actual
this.Control[iCurrent+10]=this.st_5
this.Control[iCurrent+11]=this.ddlb_lado
this.Control[iCurrent+12]=this.dw_2
this.Control[iCurrent+13]=this.gb_3
this.Control[iCurrent+14]=this.gb_4
this.Control[iCurrent+15]=this.ole_1
this.Control[iCurrent+16]=this.cbx_x5
end on

on w_info_adhesivos_cajas.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.uo_sellinea)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.em_emision)
destroy(this.uo_selsalida)
destroy(this.st_4)
destroy(this.em_actual)
destroy(this.st_5)
destroy(this.ddlb_lado)
destroy(this.dw_2)
destroy(this.gb_3)
destroy(this.gb_4)
destroy(this.ole_1)
destroy(this.cbx_x5)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelLinea.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelSalida.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelLinea.Seleccion(False,False)
	uo_SelSalida.Seleccion(False,False)
	uo_SelLinea.Filtra(gstr_paramplanta.CodigoPlanta)
	uo_SelSalida.Filtra(gstr_paramplanta.CodigoPlanta, -1)
	
	dw_1.SetTransObject(Sqlca)
	
	iuo_adhesivos	=	Create uo_correladhesivos
	
	iuo_Adhesivos.Existe(gi_CodExport, gstr_paramplanta.CodigoPlanta, True, Sqlca)
	
	em_actual.Text	= String(iuo_Adhesivos.Correlativo, '#,##0')

	ole_1.object.LicenseMe ("Mem: Exportadora Rio Blanco Santiago CL", 3, 1, "33E3AD94226C68E043BEF94763700EA9", 44)

End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_adhesivos_cajas
end type

type st_computador from w_para_informes`st_computador within w_info_adhesivos_cajas
end type

type st_usuario from w_para_informes`st_usuario within w_info_adhesivos_cajas
end type

type st_temporada from w_para_informes`st_temporada within w_info_adhesivos_cajas
end type

type p_logo from w_para_informes`p_logo within w_info_adhesivos_cajas
end type

type st_titulo from w_para_informes`st_titulo within w_info_adhesivos_cajas
integer width = 1454
string text = "Impresión de Adhesivos de Cajas"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_adhesivos_cajas
string tag = "Imprimir Reporte"
integer x = 1810
integer y = 492
integer taborder = 100
end type

event pb_acepta::clicked;Long		ll_fila_e, respuesta, ll_controlimp, ll_control
Integer	li_cantidad

li_cantidad	=	Integer(em_emision.Text)

IF MOD(li_cantidad, 5) <> 0 AND cbx_x5.Checked THEN
	MessageBox("Error de protección de datos", "Solo debe imprimir adhesivos en multiplos de 5")
	Return
ELSEIF cbx_x5.Checked THEN			
	dw_2.DataObject	=	'dw_info_adhesivos_embaladorax5'
	dw_2.SetTransObject(sqlca)
	dw_2.InsertRow(0)
END IF

Do
	ll_fila_e	=	dw_1.Retrieve(gi_CodExport, gstr_paramplanta.CodigoPlanta, uo_SelSalida.Codigo, is_Lado, Integer(em_emision.Text), Long(em_actual.Text))
										  
	If ll_fila_e = -1 Then
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)		
	ElseIf ll_fila_e < 1 Then 
		SetPointer(Arrow!)
		Return
	Else
		DO WHILE dw_1.RowCount() > 0
			IF NOT cbx_x5.Checked THEN
				dw_2.InsertRow(0)
				dw_2.InsertRow(0)
				dw_2.Object.Ole_1.Object.Text	=	dw_1.Object.codigo[1]
				
				IF dw_1.RowCount() > 1 THEN
					dw_2.Object.Ole_2.Object.Text	=	dw_1.Object.codigo[2]
					
				ELSE
					dw_2.Object.Ole_2.Object.Text	=	dw_1.Object.codigo[1]
					
				END IF
				
				IF dw_2.Print() = -1 Then 
					MessageBox("Error", "No se pudo realizar la impresión")
					SetPointer(Arrow!)
					Return
					
				END IF
				
				IF dw_1.RowCount() > 1 THEN
					dw_1.DeleteRow(2)
					
				END IF
				
				dw_1.DeleteRow(1)
				dw_2.Reset()
				
			ELSE
				
				ll_control ++
				CHOOSE CASE ll_control
					CASE 1
						dw_2.Object.Ole_1.Object.Text	=	String(dw_1.Object.codigo[1])
						
					CASE 2
						dw_2.Object.Ole_2.Object.Text	=	String(dw_1.Object.codigo[2])
						
					CASE 3
						dw_2.Object.Ole_3.Object.Text	=	String(dw_1.Object.codigo[3])
						
					CASE 4
						dw_2.Object.Ole_4.Object.Text	=	String(dw_1.Object.codigo[4])
							
					CASE 5
						dw_2.Object.Ole_5.Object.Text	=	String(dw_1.Object.codigo[5])
						ll_control							=	0
						ll_controlimp						=	dw_2.Print()
						dw_2.Reset()
						dw_2.InsertRow(0)
						
						dw_1.DeleteRow(5)
						dw_1.DeleteRow(4)
						dw_1.DeleteRow(3)
						dw_1.DeleteRow(2)
						dw_1.DeleteRow(1)
						
				END CHOOSE
			END IF
		LOOP
	END IF
Loop While respuesta = 1

iuo_Adhesivos.Existe(gi_CodExport, gstr_paramplanta.CodigoPlanta, True, Sqlca)
em_actual.Text	= String(iuo_Adhesivos.Correlativo, '#,##0')

Disconnect Using Sqlca;
Connect Using Sqlca;
dw_1.SetTransObject(Sqlca)

IF respuesta = 2 Then Close(Parent)


end event

type pb_salir from w_para_informes`pb_salir within w_info_adhesivos_cajas
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1806
integer y = 752
integer taborder = 110
string disabledname = "\desarrollo\bmp\exitd.bmp"
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type dw_1 from datawindow within w_info_adhesivos_cajas
boolean visible = false
integer x = 37
integer y = 1396
integer width = 983
integer height = 372
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_genera_adhesivo_cajas"
borderstyle borderstyle = stylelowered!
end type

type uo_sellinea from uo_seleccion_lineapacking within w_info_adhesivos_cajas
integer x = 617
integer y = 488
integer height = 84
integer taborder = 110
boolean bringtotop = true
end type

on uo_sellinea.destroy
call uo_seleccion_lineapacking::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

Choose Case This.Codigo
	Case -1, -9
		
	Case This.Codigo
		uo_SelSalida.Filtra(gstr_ParamPlanta.CodigoPlanta, This.Codigo)
		
End Choose 
end event

type st_1 from statictext within w_info_adhesivos_cajas
integer x = 320
integer y = 496
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
string text = "Linea"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_adhesivos_cajas
integer x = 320
integer y = 604
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
string text = "Salida"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_adhesivos_cajas
integer x = 325
integer y = 1100
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

type em_emision from editmask within w_info_adhesivos_cajas
integer x = 946
integer y = 1088
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

type uo_selsalida from uo_seleccion_salidapacking within w_info_adhesivos_cajas
integer x = 617
integer y = 596
integer height = 84
integer taborder = 120
boolean bringtotop = true
end type

on uo_selsalida.destroy
call uo_seleccion_salidapacking::destroy
end on

type st_4 from statictext within w_info_adhesivos_cajas
integer x = 320
integer y = 980
integer width = 402
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
string text = "Caja Actual"
boolean focusrectangle = false
end type

type em_actual from editmask within w_info_adhesivos_cajas
integer x = 946
integer y = 968
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
long backcolor = 16777215
boolean enabled = false
alignment alignment = right!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
string mask = "#,##0"
end type

type st_5 from statictext within w_info_adhesivos_cajas
integer x = 320
integer y = 720
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
string text = "Lado"
boolean focusrectangle = false
end type

type ddlb_lado from dropdownlistbox within w_info_adhesivos_cajas
integer x = 617
integer y = 716
integer width = 896
integer height = 400
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 16777215
string item[] = {"A","B","C"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;Choose Case Index
	Case 1
		is_Lado = 'A'
	Case 2
		is_Lado = 'B'
	Case 3
		is_Lado = 'C'
		
End Choose 
end event

type dw_2 from datawindow within w_info_adhesivos_cajas
boolean visible = false
integer x = 805
integer y = 164
integer width = 983
integer height = 372
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dw_genera_adhesivo_cajas"
borderstyle borderstyle = stylelowered!
end type

type gb_3 from groupbox within w_info_adhesivos_cajas
integer x = 251
integer y = 408
integer width = 1449
integer height = 460
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
end type

type gb_4 from groupbox within w_info_adhesivos_cajas
integer x = 251
integer y = 868
integer width = 1449
integer height = 400
integer taborder = 130
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
end type

type ole_1 from olecustomcontrol within w_info_adhesivos_cajas
event click ( )
event dblclick ( )
event mousedown ( integer nbutton,  integer nshift,  long ocx_x,  long ocx_y )
event mousemove ( integer nbutton,  integer nshift,  long ocx_x,  long ocx_y )
event mouseup ( integer nbutton,  integer nshift,  long ocx_x,  long ocx_y )
event beforedraw ( )
integer x = 256
integer y = 460
integer width = 247
integer height = 152
integer taborder = 130
borderstyle borderstyle = stylelowered!
long backcolor = 33543637
boolean focusrectangle = false
string binarykey = "w_info_adhesivos_cajas.win"
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type

type cbx_x5 from checkbox within w_info_adhesivos_cajas
integer x = 1367
integer y = 1096
integer width = 201
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "X5"
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
0Fw_info_adhesivos_cajas.bin 
2D00001400e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd00000006fffffffe000000040000000500000007fffffffe00000008fffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff0000000300000000000000000000000000000000000000000000000000000000561f138001d4f14d00000003000008400000000000500003004c004200430049004e0045004500530045004b000000590000000000000000000000000000000000000000000000000000000000000000000000000002001cffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000002e0000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000002001affffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000001000003e700000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000101001a00000002000000010000000485382357404fca79c1bc00b25884d19700000000561f138001d4f14d561f138001d4f14d000000000000000000000000fffffffe00000002000000030000000400000005000000060000000700000008000000090000000a0000000b0000000c0000000d0000000e0000000f00000010fffffffe00000012000000130000001400000015000000160000001700000018000000190000001a0000001b0000001c0000001d0000001e0000001f00000020fffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
2Fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0079004d00720020006e00750074002d006d0069002000650069006c006500630073006e002000650065006b00000079000000000000000000000000000000000000090000000595000003edffff0013000300ff00000000ffff000b520300098f910be3e39d11ce4b00aa00000151b8019000000001424420534d0c6c6568536c44206c0000136708000000000004000000310000000300030000000000140001000300030000000000000000000b000000080003000000ffffff00000003ff08000000000008003300330000003900000003000b0000000bffff00080000000000000030000800310000003a0042004200320033003a003a0042004200340031003a003a0053005300320033003a003a0053005300340008000000000020003a0031003a0032003a0033003a0034003a0031003a0032003a0033000000340000001300080000000000520061004c006f0020006500700061007200690063006e00f30073002000200065006f00630070006d0065006c00f30074006300200072006f0065007200740063006d0061006e0065006500740020002e00030000000000000000000b000000030003000000ffffff00000005ff00000000034047000000000000000b000000050000000000033ff0000000000000000b0000000800050000000000000000000000000003000300000000005a000000030003000000ffffff00000003ff03000000000000000000030005000000000000000000000000000500000000000500000000000000000000000000050000000000030000000000000000000300050000000000000072c0000001000340030000000000000001000300130000000000000000090000ff00030003ffffffffffff00ff0003ff03ffffffffffff00000003ff00000000030000090000000000000b00000003000b00000003000000ffffff00ff0003ff03ffffffffffff00000900ff000003000b00000003000000ffffff0000000bff0000030008000000000002000b00000012000000120041000800410000000200000000000300000900000400ff00030003ffffffffffff00ff0003ff0bffffff0800000000000200080000000000020008000000000002000800000000000600360039000000000003000009ffffff00ff0003ff03ffffffffffff00000003ff03000000ffffff00020008ff00000000ff00030003ffffffffffff00020008ff000000000200080000000000ff00030003ffffffffffff00020008ff00000000ff00030008ffffff000002000b00000003000000000000000000030000000000030000090000000000000300030000000000000002000800000000000100030003000000ffffff00ff0003ff03ffffffffffff00ff0003ff02ffffff03ffff00ffffff00000900ff00000b00ff00030003ffffffffffff00ff0003ff03ffffffffffff00010003ff03000000ffffff00000003ff000000000000000000000000000000000000000000000000000000000000090000000595000003edffff0013000300ff00000000ffff000b520300098f910be3e39d11ce4b00aa00000151b8019000000001424420534d0c6c6568536c44206c0000136708000000000004000000310000000300030000000000140001000300030000000000000000000b000000080003000000ffffff00000003ff08000000000008003300330000003900000003000b0000000bffff00080000000000000030000800310000003a0042004200320033003a003a0042004200340031003a003a0053005300320033003a003a0053005300340008000000000020003a0031003a0032003a0033003a0034003a0031003a0032003a0033000000340000001300080000000000520061004c006f0020006500700061007200690063006e00f30073002000200065006f00630070006d0065006c00f30074006300200072006f0065007200740063006d0061006e0065006500740020002e00030000000000000000000b000000030003000000ffffff00000005ff00000000034047000000000000000b000000050000000000033ff0000000000000000b0000000800050000000000000000000000000003000300000000005a000000030003000000006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001020012ffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000011000003e7000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
22000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffff00000003ff03000000000000000000030005000000000000000000000000000500000000000500000000000000000000000000050000000000030000000000000000000300050000000000000072c0000001000340030000000000000001000300130000000000000000090000ff00030003ffffffffffff00ff0003ff03ffffffffffff00000003ff00000000030000090000000000000b00000003000b00000003000000ffffff00ff0003ff03ffffffffffff00000900ff000003000b00000003000000ffffff0000000bff0000030008000000000002000b00000012000000120041000800410000000200000000000300000900000400ff00030003ffffffffffff00ff0003ff0bffffff0800000000000200080000000000020008000000000002000800000000000600360039000000000003000009ffffff00ff0003ff03ffffffffffff00000003ff03000000ffffff00020008ff00000000ff00030003ffffffffffff00020008ff000000000200080000000000ff00030003ffffffffffff00020008ff00000000ff00030008ffffff000002000b00000003000000000000000000030000000000030000090000000000000300030000000000000002000800000000000100030003000000ffffff00ff0003ff03ffffffffffff00ff0003ff02ffffff03ffff00ffffff00000900ff00000b00ff00030003ffffffffffff00ff0003ff03ffffffffffff00010003ff03000000ffffff00000003ff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1Fw_info_adhesivos_cajas.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
