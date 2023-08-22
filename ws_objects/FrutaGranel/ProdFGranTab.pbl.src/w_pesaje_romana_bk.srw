$PBExportHeader$w_pesaje_romana_bk.srw
forward
global type w_pesaje_romana_bk from window
end type
type ole_com from olecustomcontrol within w_pesaje_romana_bk
end type
type cb_setup from commandbutton within w_pesaje_romana_bk
end type
type em_kilos from editmask within w_pesaje_romana_bk
end type
type st_fondo from statictext within w_pesaje_romana_bk
end type
type pb_1 from picturebutton within w_pesaje_romana_bk
end type
type pb_elimina from picturebutton within w_pesaje_romana_bk
end type
type pb_inserta from picturebutton within w_pesaje_romana_bk
end type
type dw_1 from datawindow within w_pesaje_romana_bk
end type
type gb_1 from groupbox within w_pesaje_romana_bk
end type
type gb_2 from groupbox within w_pesaje_romana_bk
end type
type gb_3 from groupbox within w_pesaje_romana_bk
end type
type str_pesaje from structure within w_pesaje_romana_bk
end type
end forward

type str_pesaje from structure
	time		fechahora[]
	decimal { 4 }		pesaje[]
	decimal { 4 }		total
	boolean		agrega
	boolean		modifica
	str_puertacomm		puerta
end type

global type w_pesaje_romana_bk from window
integer width = 1742
integer height = 1152
windowtype windowtype = response!
long backcolor = 12632256
ole_com ole_com
cb_setup cb_setup
em_kilos em_kilos
st_fondo st_fondo
pb_1 pb_1
pb_elimina pb_elimina
pb_inserta pb_inserta
dw_1 dw_1
gb_1 gb_1
gb_2 gb_2
gb_3 gb_3
end type
global w_pesaje_romana_bk w_pesaje_romana_bk

type variables
Long	il_fila, il_medioseg
Double	id_kilos
Integer	ii_pesajenuevo
Boolean	ib_ocx

Private:
str_pesaje	wstr_pesaje
str_puertacomm	istr_puertacomm
end variables

on w_pesaje_romana_bk.create
this.ole_com=create ole_com
this.cb_setup=create cb_setup
this.em_kilos=create em_kilos
this.st_fondo=create st_fondo
this.pb_1=create pb_1
this.pb_elimina=create pb_elimina
this.pb_inserta=create pb_inserta
this.dw_1=create dw_1
this.gb_1=create gb_1
this.gb_2=create gb_2
this.gb_3=create gb_3
this.Control[]={this.ole_com,&
this.cb_setup,&
this.em_kilos,&
this.st_fondo,&
this.pb_1,&
this.pb_elimina,&
this.pb_inserta,&
this.dw_1,&
this.gb_1,&
this.gb_2,&
this.gb_3}
end on

on w_pesaje_romana_bk.destroy
destroy(this.ole_com)
destroy(this.cb_setup)
destroy(this.em_kilos)
destroy(this.st_fondo)
destroy(this.pb_1)
destroy(this.pb_elimina)
destroy(this.pb_inserta)
destroy(this.dw_1)
destroy(this.gb_1)
destroy(this.gb_2)
destroy(this.gb_3)
end on

event open;Integer	li_resultado
Long		ll_elemento, ll_Fila
String	ls_parametros

wstr_pesaje	=	Message.PowerObjectParm

dw_1.SetTransObject(SQLCA)

//FOR ll_Elemento	=	1 TO UpperBound(wstr_pesaje.fechahora)
//	ll_Fila	=	dw_1.InsertRow(0)
//	
//	dw_1.Object.horaevento[ll_Fila]	=	wstr_pesaje.fechahora[ll_Elemento]
//	dw_1.Object.pesaje[ll_Fila]		=	wstr_pesaje.pesaje[ll_Elemento]
//	
////NEXT
//
pb_inserta.SetFocus()
IF dw_1.RowCount() > 0 THEN
	pb_elimina.Enabled	=	True
END IF

li_resultado 	=	ConfiguracionPuerta(istr_puertacomm)

IF li_resultado = 0 THEN
	ls_parametros = "4800,n,	8,"+ "1" 
//	ls_parametros	=	"baud="+String(istr_puertacomm.Baudios)+" "+&
//							"parity="+istr_puertacomm.Paridad+" "+&
//							"data="+String(istr_puertacomm.Data)+" "+&
//							"stop="+String(istr_puertacomm.Parada)
							
//	IF RegistryGet( &
//		"HKEY_LOCAL_MACHINE\Software\Classes\IO.IOCtrl.1", &
//			"", RegString!, ls_nombre) = 1 THEN
//		IF ls_Nombre = "IO Control" THEN ib_OCX	=	True
//	END IF

 ib_OCX	=	True
 
	IF NOT ib_OCX THEN
		MessageBox("Conexión Romana","No está instalado el OCX para conexión con Romana")
	ELSE
		li_resultado 	= Ole_com.object.Open(istr_puertacomm.Puerta, ls_parametros)
	
		IF li_Resultado = 0 THEN
			MessageBox("Conexión Romana","No es posible conectar con romana")
		ELSE
			IF istr_puertacomm.PesoMinimo = 0 THEN
				istr_puertacomm.PesoMinimo = 0.5
			END IF
			
			IF istr_puertacomm.LargoLectura = 0 THEN
				istr_puertacomm.LargoLectura = 8
			END IF
			
			IF istr_puertacomm.LargoCadena = 0 THEN
				istr_puertacomm.LargoCadena = 8
			END IF
		END IF	
	END IF
END IF
istr_puertacomm	=	wstr_pesaje.puerta

//IF w_maed_movtofrutagranel_recepcion.ib_OCX THEN
	Timer(0.5)
//END IF
end event

event timer;//Integer	li_factor, li_posini
String 	ls_string
//Double	ld_kilos
//
//ls_string = w_maed_movtofrutagranel_recepcion.Ole_Puerta.Object.ReadString(istr_puertacomm.LargoLectura) 
//
//li_posini = Pos(ls_string,istr_puertacomm.CadenaInicio) + Len(istr_puertacomm.CadenaInicio)
//
//IF Len(Mid(ls_string,li_posini)) < istr_puertacomm.LargoCadena THEN RETURN
//	
//IF IsNumber(Mid(ls_string,li_posini,istr_puertacomm.LargoCadena)) THEN
//	ld_kilos	=	Dec(Mid(ls_string,li_posini,istr_puertacomm.LargoCadena))
//	IF istr_puertacomm.Decimales > 0 THEN
//		li_factor	= 10 ^ istr_puertacomm.Decimales
//		ld_kilos		= Round(ld_kilos/li_factor,istr_puertacomm.Decimales)
//	END IF
//	em_kilos.Text	=	String(ld_kilos)
//END IF
//
//IF ld_kilos < istr_puertacomm.PesoMinimo THEN
//	ii_pesajenuevo	=	0
//	RETURN
//END IF
//
//IF ii_pesajenuevo = 1 THEN 
//	RETURN
//END IF
//
//IF ld_kilos = id_kilos THEN
//	il_medioseg ++
//ELSE
//	id_kilos		=	ld_kilos
//	il_medioseg =	1
//END IF
//
//IF il_medioseg / 2 >= istr_puertacomm.Estabilidad THEN
//	pb_elimina.Enabled	=	True
//	
//	il_Fila	=	dw_1.InsertRow(0)
//	
//	dw_1.Object.horaevento[il_fila]	=	Time(Now())
//	dw_1.Object.pesaje[il_fila]		=	ld_kilos
//	dw_1.SetRow(il_Fila)
//	
//	dw_1.SetColumn("pesaje")
//	ii_pesajenuevo	=	1
//	il_medioseg		=	1
//END IF


int bufflen
string car

bufflen = ole_com.object.InBufferCount

if bufflen > 0 then
	car = ole_com.object.input
	ls_string = Mid(car,2,7)
	em_kilos.text	=	String(Dec(ls_string)/10)
end if
end event

type ole_com from olecustomcontrol within w_pesaje_romana_bk
event oncomm ( )
integer x = 1431
integer y = 72
integer width = 174
integer height = 152
integer taborder = 30
borderstyle borderstyle = stylelowered!
long backcolor = 16711680
boolean focusrectangle = false
string binarykey = "w_pesaje_romana_bk.win"
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
end type

type cb_setup from commandbutton within w_pesaje_romana_bk
integer x = 878
integer y = 100
integer width = 288
integer height = 92
integer taborder = 20
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Setup Com."
end type

event clicked;//Integer	li_resultado
////Obtiene Seteo de la Puerta
//w_maed_movtofrutagranel_recepcion.ole_puerta.Object.SerialGetPortDefaults (istr_puertacomm.Puerta)     
////Permite Seteo al Usuario
//li_resultado	= w_maed_movtofrutagranel_recepcion.ole_puerta.Object.SerialPortSetupDialog(istr_puertacomm.Puerta)
////Graba el Seteo
//If li_resultado = 1 Then 
//    li_resultado	= w_maed_movtofrutagranel_recepcion.ole_puerta.Object.SerialSetPortDefaults(istr_puertacomm.Puerta, "", -1)
//End If

string pars

pars = "4800" + /*ddlb_bauds.text+ */"," + "n,"
//CHOOSE CASE ddlb_paridad.text
//	CASE "ninguna"
//		pars = pars+"n,"
//	CASE "par"
//		pars = pars+"p,"
//	CASE "impar"
//		pars = pars+"e,"
//END CHOOSE

pars = pars+ "8" + /*ddlb_long.text*/+","+ "1" /*ddlb_bits.text*/

if ole_com.object.PortOpen then ole_com.object.PortOpen = false
ole_com.object.settings = pars
ole_com.object.PortOpen = True
//pb_iniciar.enabled = true
//pb_iniciar.picturename = "rayo.bmp"
//pb_transmite.enabled = true
end event

type em_kilos from editmask within w_pesaje_romana_bk
integer x = 78
integer y = 84
integer width = 667
integer height = 120
integer taborder = 40
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
string text = "none"
alignment alignment = right!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
string mask = "#,##0.00"
end type

type st_fondo from statictext within w_pesaje_romana_bk
integer x = 41
integer y = 52
integer width = 745
integer height = 188
integer textsize = -20
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "System"
long textcolor = 33554432
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_1 from picturebutton within w_pesaje_romana_bk
integer x = 1381
integer y = 860
integer width = 155
integer height = 132
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\exit.bmp"
alignment htextalign = left!
end type

event clicked;Long			ll_elemento, ll_Fila
str_pesaje	lstr_pesaje

FOR ll_Fila	=	1 TO dw_1.RowCount()
	ll_Elemento	++
	
	IF dw_1.Object.pesaje[ll_Fila] > 0 THEN
		lstr_pesaje.fechahora[ll_Elemento]	=	dw_1.Object.horaevento[ll_Fila]
		lstr_pesaje.pesaje[ll_Elemento]		=	dw_1.Object.pesaje[ll_Fila]
	END IF
	
NEXT

IF dw_1.RowCount() > 0 THEN
	lstr_pesaje.total	=	dw_1.Object.total_romana[1]
END IF

CloseWithReturn(Parent,lstr_pesaje)
end event

type pb_elimina from picturebutton within w_pesaje_romana_bk
integer x = 1381
integer y = 528
integer width = 155
integer height = 132
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\desarrollo\bmp\suprime.bmp"
string disabledname = "\desarrollo\bmp\suprimd.bmp"
alignment htextalign = left!
end type

event clicked;IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		MessageBox(Parent.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_1.RowCount() = 0 THEN
		This.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

type pb_inserta from picturebutton within w_pesaje_romana_bk
integer x = 1381
integer y = 348
integer width = 155
integer height = 132
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\desarrollo\bmp\inserte.bmp"
string disabledname = "\desarrollo\bmp\insertd.bmp"
alignment htextalign = left!
end type

event clicked;pb_elimina.Enabled	=	True

il_Fila	=	dw_1.InsertRow(0)

dw_1.Object.horaevento[il_fila]	=	Time(Now())
dw_1.Object.pesaje[il_fila]	=	Dec(em_kilos.text)
dw_1.SetRow(il_Fila)

dw_1.SetColumn("pesaje")
dw_1.SetFocus()
end event

type dw_1 from datawindow within w_pesaje_romana_bk
integer x = 37
integer y = 268
integer width = 1189
integer height = 788
integer taborder = 10
boolean titlebar = true
string title = "Pesaje de Romana"
string dataobject = "dw_pesaje_romana"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event itemerror;RETURN 1
end event

type gb_1 from groupbox within w_pesaje_romana_bk
integer x = 1317
integer y = 260
integer width = 288
integer height = 456
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_2 from groupbox within w_pesaje_romana_bk
integer x = 1317
integer y = 776
integer width = 288
integer height = 272
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_3 from groupbox within w_pesaje_romana_bk
integer x = 818
integer y = 24
integer width = 407
integer height = 220
integer taborder = 30
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
0Ew_pesaje_romana_bk.bin 
2600000c00e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd00000004fffffffefffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff00000003000000000000000000000000000000000000000000000000000000008058b6b001c6753200000003000000c00000000000500003004c004200430049004e0045004500530045004b000000590000000000000000000000000000000000000000000000000000000000000000000000000002001cffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000260000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000002001affffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000010000003c00000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000101001a000000020000000100000004648a5600101b2c6e0000b68214000000000000008058b6b001c675328058b6b001c67532000000000000000000000000fffffffefffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
2Affffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f0043007900700069007200680067002000740063002800200029003900310034003900720020006e006100360030003000310070002e006c0062004300001234432100000008000003ed000003ed648a560100060000000100000000040000000200000025800008000000000000000000000000003f00000001004700461234432100000008000003ed000003ed648a560100060000000100000000040000000200000025800008000000000000000000000000003f0000000100720072006c006f006f006c0054005c006200610061006c00500073006f0072005c006400610054006c00620073006100470046006100720065006e005c006c006100540046006200720047006e006100380030003000310070002e006c006200430000005c003a006500440061007300720072006c006f006f006c0041005c006d0064007300550061007500310072002e0030006200700000006c003a00430044005c0073006500720061006f0072006c006c005c006f0062004f0065006a00750050006c0062003000310070002e006c00627fff000000007fff00000000020ee748000000000000000000000000000000000000000000000000000000000000000000b90009000c0000001301c0001b7b987fffffff7fffffff7fff7fff00007fff00000000020ee760000000000000000000000000000000000000000000000000006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001020012ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000020000003c000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1Ew_pesaje_romana_bk.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
