$PBExportHeader$w_info_adhesivos_genericos.srw
forward
global type w_info_adhesivos_genericos from w_para_informes
end type
type tit_peso from statictext within w_info_adhesivos_genericos
end type
type gb_3 from groupbox within w_info_adhesivos_genericos
end type
type dw_1 from datawindow within w_info_adhesivos_genericos
end type
type ole_1 from olecustomcontrol within w_info_adhesivos_genericos
end type
type dw_2 from datawindow within w_info_adhesivos_genericos
end type
end forward

global type w_info_adhesivos_genericos from w_para_informes
integer x = 14
integer y = 32
integer width = 2665
integer height = 1156
string title = "IMPRESION DE ADHESIVOS"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
tit_peso tit_peso
gb_3 gb_3
dw_1 dw_1
ole_1 ole_1
dw_2 dw_2
end type
global w_info_adhesivos_genericos w_info_adhesivos_genericos

type variables
str_mant				istr_mant
uo_entidades		iuo_entidades
DataWindowChild	idwc_formato

Integer				ii_cantidad, ii_planta, ii_Tipo
Long					il_embaladora
String				is_abreviado
end variables

forward prototypes
protected function boolean wf_actualiza_db ()
public subroutine cargadatos ()
public function string carganombre (long al_tarjeta)
end prototypes

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

//IF dw_2.Update(True, False) = 1 then 
//	Commit;
//	
//	IF sqlca.SQLCode <> 0 THEN
//		F_ErrorBaseDatos(sqlca, This.Title)
//		lb_Retorno	=	False
//	ELSE
		lb_Retorno	=	True
//			
//		dw_2.ResetUpdate()
//	END IF
//ELSE
//	RollBack;
//	
//	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
//	
//	lb_Retorno	=	False
//END IF
//
sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine cargadatos ();Integer	li_fila, respuesta, li_tipo
end subroutine

public function string carganombre (long al_tarjeta);String	ls_nombre

//IF al_tarjeta = 0 OR IsNull(al_tarjeta) THEN Return ''
//
//select pers_nombre + ' ' + pers_apepat + ' ' + pers_apemat
//  into :ls_nombre
//  from dba.remupersonal
// where pers_nrotar = :al_tarjeta;
// 
//IF sqlca.SQLCode <> 0 THEN
//	F_ErrorBaseDatos(sqlca, "Problemas con la carga del nombre del personal")
//	RETURN ''
//END IF

RETURN ls_nombre
end function

on w_info_adhesivos_genericos.create
int iCurrent
call super::create
this.tit_peso=create tit_peso
this.gb_3=create gb_3
this.dw_1=create dw_1
this.ole_1=create ole_1
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tit_peso
this.Control[iCurrent+2]=this.gb_3
this.Control[iCurrent+3]=this.dw_1
this.Control[iCurrent+4]=this.ole_1
this.Control[iCurrent+5]=this.dw_2
end on

on w_info_adhesivos_genericos.destroy
call super::destroy
destroy(this.tit_peso)
destroy(this.gb_3)
destroy(this.dw_1)
destroy(this.ole_1)
destroy(this.dw_2)
end on

event open;call super::open;Integer				li_fila, respuesta

dw_2.SetTransObject(sqlca)
dw_2.GetChild("formato", idwc_formato)
idwc_formato.SetTransObject(SQLCA)
idwc_formato.Retrieve(0)

dw_2.InsertRow(0)

SetNull(ii_cantidad)

CargaDatos()
end event

type pb_excel from w_para_informes`pb_excel within w_info_adhesivos_genericos
end type

type st_computador from w_para_informes`st_computador within w_info_adhesivos_genericos
integer x = 1019
integer y = 148
integer width = 1445
end type

type st_usuario from w_para_informes`st_usuario within w_info_adhesivos_genericos
integer x = 1019
integer width = 1445
end type

type st_temporada from w_para_informes`st_temporada within w_info_adhesivos_genericos
integer x = 1019
integer y = 4
integer width = 1445
end type

type p_logo from w_para_informes`p_logo within w_info_adhesivos_genericos
end type

type st_titulo from w_para_informes`st_titulo within w_info_adhesivos_genericos
integer width = 1801
string text = "Impresión de Adhesivos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_adhesivos_genericos
string tag = "Imprimir Reporte"
integer x = 2149
integer y = 444
integer taborder = 100
end type

event pb_acepta::clicked;Long		fila, ll_cantidad, li_ole, li_fila
String	ls_codigo


SetPointer(HourGlass!)

dw_2.AcceptText()
dw_1.Reset()

ll_cantidad			=	dw_2.Object.cantidad[1]
dw_1.DataObject	=	dw_2.Object.formato[1]

IF dw_2.Object.formato[1] = "dw_adhesivo_ventana_pallet_60x20" THEN
	ls_codigo	=	dw_2.Object.texto_a_imprimir[1]

	FOR fila = 1 TO ll_cantidad
		
		li_fila								=	dw_1.InsertRow(0)
		dw_1.Object.Codigo[li_fila]	=	ls_codigo
		
		IF MOD(li_fila, 4) = 0 OR fila = ll_cantidad THEN
			dw_1.Print()
			dw_1.Reset()
		END IF
	
	NEXT
ELSE
	ls_codigo	=	Upper(dw_2.Object.codigo_a_imprimir[1])
	
	IF dw_2.Object.variable[1] = "cali" THEN
		ls_codigo	=	ls_codigo + Right('   ', 3 - Len(ls_codigo))
	END IF
	
	li_ole		=	0
	
	FOR fila = 1 TO ll_cantidad
		li_fila	=	dw_1.InsertRow(0)
		li_ole	++
		
		IF dw_2.Object.variable[1] = "vari" THEN
			dw_1.Object.Codigo[li_fila]		=	dw_2.Object.texto_a_imprimir[1]
		ELSE
			dw_1.Object.Codigo[li_fila]		=	ls_codigo
		END IF
		
		CHOOSE CASE li_ole
			CASE 1
				dw_1.Object.Ole_1.Object.Text = 	ls_codigo
				
			CASE 2
				dw_1.Object.Ole_2.Object.Text = 	ls_codigo
				
			CASE 3
				dw_1.Object.Ole_3.Object.Text = 	ls_codigo
				li_ole								=	0
				
		END CHOOSE
		
		IF MOD(li_fila, 3) = 0 OR fila = ll_cantidad THEN
			dw_1.Print()
			dw_1.Reset()
		END IF
	
	NEXT
END IF
end event

type pb_salir from w_para_informes`pb_salir within w_info_adhesivos_genericos
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2149
integer y = 704
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type tit_peso from statictext within w_info_adhesivos_genericos
boolean visible = false
integer x = 489
integer y = 2352
integer width = 183
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Peso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type gb_3 from groupbox within w_info_adhesivos_genericos
boolean visible = false
integer x = 96
integer y = 2020
integer width = 1614
integer height = 280
integer taborder = 120
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type dw_1 from datawindow within w_info_adhesivos_genericos
boolean visible = false
integer x = 59
integer y = 704
integer width = 1134
integer height = 492
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_adhesivo_ventana_pallet_70x30"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type ole_1 from olecustomcontrol within w_info_adhesivos_genericos
event click ( )
event dblclick ( )
event mousedown ( integer nbutton,  integer nshift,  long ocx_x,  long ocx_y )
event mousemove ( integer nbutton,  integer nshift,  long ocx_x,  long ocx_y )
event mouseup ( integer nbutton,  integer nshift,  long ocx_x,  long ocx_y )
event beforedraw ( )
boolean visible = false
integer x = 1239
integer y = 716
integer width = 389
integer height = 200
integer taborder = 130
boolean bringtotop = true
boolean border = false
boolean focusrectangle = false
string binarykey = "w_info_adhesivos_genericos.win"
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type

type dw_2 from datawindow within w_info_adhesivos_genericos
integer x = 251
integer y = 444
integer width = 1792
integer height = 456
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_adhesivos_genericos"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_fila, respuesta, ls_nula
String		ls_columna

SetNull(ls_nula)

ls_columna	=	dwo.name

CHOOSE CASE ls_columna
	CASE "coad_embala"
		IF NOT iuo_entidades.existeembaladora(Long(data), TRUE, sqlca) Then
			dw_2.Object.coad_embala[1] 	=	Long(ls_nula)
			dw_2.Object.pers_nombre[1] 	=	String(ls_nula)
			Return 1
		ELSE
			il_embaladora	=	Long(data)
			CargaDatos()
		END IF
		
END CHOOSE
end event

event itemerror;Return 1
end event

event buttonclicked;String			ls_columna, ls_busqueda, ls_codigo
Str_busqueda	lstr_busq

ls_columna	=	dwo.name

CHOOSE CASE ls_columna
	CASE "b_busqueda"
		ls_busqueda	=	dw_2.Object.variable[1] 
		
		CHOOSE CASE ls_busqueda
			CASE "espe"
				OpenWithParm(w_busc_especies_gpp, lstr_busq)
				lstr_busq	=	Message.PowerObjectParm
				
				IF UpperBound(lstr_busq.argum) > 1 THEN
					dw_2.Object.texto_a_imprimir[1]	=	lstr_busq.argum[3]
				END IF
				
			CASE "vari"
				lstr_busq.argum[1]	=	'0'
				lstr_busq.argum[2]	=	""
				
				OpenWithParm(w_busc_variedades, lstr_busq)
				lstr_busq	=	Message.PowerObjectParm
				
				IF UpperBound(lstr_busq.argum) > 1 THEN
					ls_codigo								=  Right("00", 2 - Len(lstr_busq.argum[1])) + lstr_busq.argum[1]
					ls_codigo				=	ls_codigo + Right("0000", 4 - Len(lstr_busq.argum[3])) + lstr_busq.argum[3]
					
					dw_2.Object.codigo_a_imprimir[1]	=	ls_codigo
					dw_2.Object.texto_a_imprimir[1]	=	lstr_busq.argum[4]
				END IF
				
			CASE "prod"
				OpenWithParm(w_busc_productores, lstr_busq)
				lstr_busq	=	Message.PowerObjectParm
				
				IF UpperBound(lstr_busq.argum) > 1 THEN
					dw_2.Object.codigo_a_imprimir[1]	=	lstr_busq.argum[1]
				END IF
				
		END CHOOSE
		
END CHOOSE
end event


Start of PowerBuilder Binary Data Section : Do NOT Edit
0Cw_info_adhesivos_genericos.bin 
2100001400e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd00000006fffffffe000000040000000500000007fffffffe00000008fffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff000000030000000000000000000000000000000000000000000000000000000031972fe001d4f14b00000003000008400000000000500003004c004200430049004e0045004500530045004b000000590000000000000000000000000000000000000000000000000000000000000000000000000002001cffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000002e0000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000002001affffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000001000003f100000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000101001a00000002000000010000000485382357404fca79c1bc00b25884d1970000000031972fe001d4f14b31972fe001d4f14b000000000000000000000000fffffffe00000002000000030000000400000005000000060000000700000008000000090000000a0000000b0000000c0000000d0000000e0000000f00000010fffffffe00000012000000130000001400000015000000160000001700000018000000190000001a0000001b0000001c0000001d0000001e0000001f00000020fffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
2Fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0079004d00720020006e00750074002d006d0069002000650069006c006500630073006e002000650065006b000000790000000000000000000000000000000000000900000008c90000052bffff0013000300ff00000000ffff000b520300098f910be3e39d11ce4b00aa00000151b8019000000001424420534d0c6c6568536c44206c000013670800000000000e007300610061006400640073000300000000000000140003000300000000000100000003000b0000000800000000000000ff00030003ffffff000000000800080033000000390033000300000000000000ff000b0000000bff0000080008000000000030004200310032003a003a0042004200330034003a003a0042005300310032003a003a0053005300330034003a0000005300200008003100000032003a0033003a0034003a0031003a0032003a0033003a0034003a001300000000000000520008004c0000002000610070006f007200650063006100f300690020006e0065007300630020006d006f006c007000740065002000f3006f00630072007200630065006100740065006d0074006e002e00650000002000000003000b0000000300000000000000ff00030005ffffff0000000059400000000003400b0000000500000000000000f00000000000033f0b000000080000000000000000000500000000000300000000000000520003000300000000000000ff00030003ffffff00000000000003000300000000000000000005000000000005000000000000000000000000000500000000000500000000000000000000000000030003000000000000000000050000000000034072c000000100000003000300000000000100000013000000000003000009ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff0000000000090000000003000b000000030000000000000000000b00ff00030003ffffffffffff00ff0003ff00ffffff030000090000000000000b00ff0003000bffffff0300000000000000020008000000000000000b0041001200410012000200080000000000000900000400030003000000ffffff00ff0003ff03ffffffffffff0000000bff02000800000000000200080000000000020008000000000006000800390000000000360000090000ff00030003ffffffffffff00ff0003ff03ffffff00000000ff00030008ffffff0000020003000000ffffff00ff0003ff08ffffff00000200080000000000020003000000ffffff00ff0003ff08ffffff0000020003000000ffffff00020008ff0000000000000b00000003000300000000000000000900000000030003000000000000000000030008000000000002000300000000000100ff00030003ffffffffffff00ff0003ff03ffffffffffff00ff0002ffff0003ff00ffffff0b00000903000000ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff00000100ff00030003ffffff000000000000000000000000000000000000000000000900000008c90000052bffff0013000300ff00000000ffff000b520300098f910be3e39d11ce4b00aa00000151b8019000000001424420534d0c6c6568536c44206c000013670800000000000e007300610061006400640073000300000000000000140003000300000000000100000003000b0000000800000000000000ff00030003ffffff000000000800080033000000390033000300000000000000ff000b0000000bff0000080008000000000030004200310032003a003a0042004200330034003a003a0042005300310032003a003a0053005300330034003a0000005300200008003100000032003a0033003a0034003a0031003a0032003a0033003a0034003a001300000000000000520008004c0000002000610070006f007200650063006100f300690020006e0065007300630020006d006f006c007000740065002000f3006f00630072007200630065006100740065006d0074006e002e00650000002000000003000b0000000300000000000000ff00030005ffffff0000000059400000000003400b0000000500000000000000f00000000000033f0b00000008000000000000000000050000000000030000000000000052000300006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001020012ffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000011000003f1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
28000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000300000000000000ff00030003ffffff00000000000003000300000000000000000005000000000005000000000000000000000000000500000000000500000000000000000000000000030003000000000000000000050000000000034072c000000100000003000300000000000100000013000000000003000009ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff0000000000090000000003000b000000030000000000000000000b00ff00030003ffffffffffff00ff0003ff00ffffff030000090000000000000b00ff0003000bffffff0300000000000000020008000000000000000b0041001200410012000200080000000000000900000400030003000000ffffff00ff0003ff03ffffffffffff0000000bff02000800000000000200080000000000020008000000000006000800390000000000360000090000ff00030003ffffffffffff00ff0003ff03ffffff00000000ff00030008ffffff0000020003000000ffffff00ff0003ff08ffffff00000200080000000000020003000000ffffff00ff0003ff08ffffff0000020003000000ffffff00020008ff0000000000000b00000003000300000000000000000900000000030003000000000000000000030008000000000002000300000000000100ff00030003ffffffffffff00ff0003ff03ffffffffffff00ff0002ffff0003ff00ffffff0b00000903000000ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff00000100ff00030003ffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1Cw_info_adhesivos_genericos.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
