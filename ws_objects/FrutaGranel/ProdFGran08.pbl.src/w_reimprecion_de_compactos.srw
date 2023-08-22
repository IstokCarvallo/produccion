$PBExportHeader$w_reimprecion_de_compactos.srw
$PBExportComments$Emisión Solicitud de Inspección.
forward
global type w_reimprecion_de_compactos from w_para_informes
end type
type st_1 from statictext within w_reimprecion_de_compactos
end type
type dw_cliente from datawindow within w_reimprecion_de_compactos
end type
type st_2 from statictext within w_reimprecion_de_compactos
end type
type dw_plantadesp from datawindow within w_reimprecion_de_compactos
end type
type em_numero from editmask within w_reimprecion_de_compactos
end type
type st_3 from statictext within w_reimprecion_de_compactos
end type
type st_5 from statictext within w_reimprecion_de_compactos
end type
type dw_16 from datawindow within w_reimprecion_de_compactos
end type
type rb_1 from radiobutton within w_reimprecion_de_compactos
end type
type rb_2 from radiobutton within w_reimprecion_de_compactos
end type
type dw_1 from datawindow within w_reimprecion_de_compactos
end type
type em_1 from editmask within w_reimprecion_de_compactos
end type
type st_4 from statictext within w_reimprecion_de_compactos
end type
type st_6 from statictext within w_reimprecion_de_compactos
end type
type em_camara from editmask within w_reimprecion_de_compactos
end type
type ole_1 from olecustomcontrol within w_reimprecion_de_compactos
end type
end forward

global type w_reimprecion_de_compactos from w_para_informes
integer width = 2487
integer height = 1068
string title = "REIMPRESION DE COMPACTOS"
boolean minbox = false
event ue_validapassword ( )
st_1 st_1
dw_cliente dw_cliente
st_2 st_2
dw_plantadesp dw_plantadesp
em_numero em_numero
st_3 st_3
st_5 st_5
dw_16 dw_16
rb_1 rb_1
rb_2 rb_2
dw_1 dw_1
em_1 em_1
st_4 st_4
st_6 st_6
em_camara em_camara
ole_1 ole_1
end type
global w_reimprecion_de_compactos w_reimprecion_de_compactos

type variables
Str_mant				istr_mant
DataWindowChild	dwc_plantas, idwc_clientes

uo_Clientesprod		iuo_cliente

Integer				ii_planta, ii_cliente
Long					il_NroPallet, il_NroCaja
String				is_Computador

end variables

forward prototypes
public function boolean wf_actualiza_db ()
end prototypes

event ue_validapassword();istr_mant.Argumento[1]	=	"Produccion"
istr_mant.Argumento[2]	=	gs_Password+''+'contraparte'

OpenWithParm(w_password, istr_mant)

istr_mant	=	Message.PowerObjectParm

IF istr_mant.Respuesta = 0 THEN Close(This)
end event

public function boolean wf_actualiza_db ();return True
end function

on w_reimprecion_de_compactos.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.dw_plantadesp=create dw_plantadesp
this.em_numero=create em_numero
this.st_3=create st_3
this.st_5=create st_5
this.dw_16=create dw_16
this.rb_1=create rb_1
this.rb_2=create rb_2
this.dw_1=create dw_1
this.em_1=create em_1
this.st_4=create st_4
this.st_6=create st_6
this.em_camara=create em_camara
this.ole_1=create ole_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_plantadesp
this.Control[iCurrent+5]=this.em_numero
this.Control[iCurrent+6]=this.st_3
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.dw_16
this.Control[iCurrent+9]=this.rb_1
this.Control[iCurrent+10]=this.rb_2
this.Control[iCurrent+11]=this.dw_1
this.Control[iCurrent+12]=this.em_1
this.Control[iCurrent+13]=this.st_4
this.Control[iCurrent+14]=this.st_6
this.Control[iCurrent+15]=this.em_camara
this.Control[iCurrent+16]=this.ole_1
end on

on w_reimprecion_de_compactos.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.dw_plantadesp)
destroy(this.em_numero)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.dw_16)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.dw_1)
destroy(this.em_1)
destroy(this.st_4)
destroy(this.st_6)
destroy(this.em_camara)
destroy(this.ole_1)
end on

event open;call super::open;dw_cliente.SetTransObject(Sqlca)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

ole_1.object.LicenseMe ("Mem: Exportadora Rio Blanco Santiago CL", 3, 1, "33E3AD94226C68E043BEF94763700EA9", 44)


IF gi_CodExport = 590 THEN
	em_camara.Enabled = True
ELSE	
	em_camara.Enabled = False
END IF	

dw_plantadesp.GetChild("plde_codigo", dwc_plantas)
dwc_plantas.SetTransObject(Sqlca)
dwc_plantas.Retrieve(1)			//Plantas de Despacho

dw_plantadesp.SetTransObject(Sqlca)
dw_plantadesp.InsertRow(0)
dw_plantadesp.SetItem(1,"plde_codigo", gi_CodPlanta)

istr_mant.Argumento[1]	=	String(gi_CodExport)
istr_mant.Argumento[2]	=	String(gi_CodPlanta)
iuo_cliente					=	Create uo_Clientesprod 

dw_16.SetTransObject(Sqlca)
dw_1.SetTransObject(Sqlca)

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", &
				"ComputerName", RegString!, is_Computador)
end event

event resize;//
end event

type st_titulo from w_para_informes`st_titulo within w_reimprecion_de_compactos
integer x = 82
integer y = 68
integer width = 1989
string text = "Reimpresión de Compactos"
end type

type pb_acepta from w_para_informes`pb_acepta within w_reimprecion_de_compactos
integer x = 2167
integer y = 316
integer taborder = 50
end type

event pb_acepta::clicked;Long	ll_pallet, ll_caja, ll_FilaPallet, ll_NroPallet, ll_Registros, ll_FilaCajas, ll_Fila, ll_cont, ll_Fila1
Integer	li_planta
String	ls_dw, ls_camara, ls_fecha

li_planta = Integer(istr_mant.argumento[2])

SELECT loco_dwcomp
INTO	:ls_dw
FROM DBA.spro_correlcompequipo
WHERE plde_codigo = :li_planta
AND	equi_nombre = :is_Computador;

dw_16.DataObject = ls_dw

dw_16.SetTransObject(Sqlca)

ll_caja = Long(em_1.Text)
IF isnull(ll_caja) OR ll_caja = 0 THEN
	ll_caja = -1
END IF

ll_pallet = Long(em_numero.Text)
IF isnull(ll_pallet) OR ll_pallet = 0 THEN
	ll_pallet = -1
END IF

ll_cont = dw_1.Retrieve(Integer(istr_mant.argumento[1]),ll_pallet,Integer(istr_mant.argumento[2]),ll_caja)

				
IF ll_cont = 0 THEN
	MessageBox( "Atención", &
						"No Existe Información para este Pallet o Caja.", &
						StopSign!, OK!)
END IF	

ls_camara = em_camara.Text

FOR ll_FilaPallet = 1 TO dw_1.RowCount()
          			
	ll_NroPallet	=	dw_1.Object.paen_numero[ll_FilaPallet]
	il_NroPallet	=	ll_NroPallet
	il_NroCaja		=	dw_1.Object.pafr_secuen[ll_FilaPallet]
	
	dw_16.Reset()
	
	IF dw_16.DataObject = "dw_info_spro_cajasprod_pomaceas"  THEN
		dw_16.SetTransObject(sqlca)
		ll_Fila = dw_16.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),il_NroCaja,ls_camara)
		
		FOR ll_Fila1 = 1 TO dw_16.RowCount()
		  dw_16.Object.envo_descrip.visible  = 1
		NEXT
	ELSE
		dw_16.SetTransObject(sqlca)
		ll_Fila = dw_16.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),il_NroCaja,il_NroCaja,-1)
	END IF

	IF ll_Fila = -1 THEN
		MessageBox( "Error en Base de Datos", &
						"Se ha producido un error en Base " + &
						"de Datos : ~n" + sqlca.SQLErrText, StopSign!, OK!)
	ELSEIF ll_Fila = 0 THEN
		MessageBox( "Atención", &
						"No Existe Información para este Pallet o Caja.", &
						StopSign!, OK!)
	ELSE
		iuo_cliente.Existe(Integer(istr_mant.argumento[1]), True, SQLCa)
		dw_16.Object.Ole_1.Object.BarCode	=	iuo_cliente.clie_codbar
		IF dw_16.Object.Ole_1.Object.BarCode = 20 THEN
			dw_16.Object.Ole_1.Object.Text 	= 	String(il_NroCaja) + String(il_NroCaja)
			
		ELSEIF dw_16.Object.Ole_1.Object.BarCode = 88 THEN
			ls_fecha									=	String(dw_16.Object.capr_fecemb[1])
			ls_fecha									=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)
			dw_16.Object.Ole_1.Object.Text 	= 	"01" + dw_16.Object.emba_nroint[1] + "10" + ls_fecha + "\F" + &
															"21" + String(dw_16.Object.plde_codigo[1], "0000") + &
															String(il_NroCaja, '00000000')
			
		END IF
		
		dw_16.Print()
		
	END IF		  
NEXT
end event

type pb_salir from w_para_informes`pb_salir within w_reimprecion_de_compactos
integer x = 2171
integer y = 592
integer taborder = 60
end type

type st_1 from statictext within w_reimprecion_de_compactos
integer x = 174
integer y = 392
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from datawindow within w_reimprecion_de_compactos
integer x = 462
integer y = 376
integer width = 1253
integer height = 96
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1] = data
	dwc_plantas.Retrieve(1)	
	
	IF istr_mant.argumento[1]  = '590' THEN
		em_camara.Enabled = True
	ELSE	
		em_camara.Enabled = False
		em_camara.Text = ''
	END IF	

	
ELSE
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_reimprecion_de_compactos
integer x = 174
integer y = 524
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Planta"
boolean focusrectangle = false
end type

type dw_plantadesp from datawindow within w_reimprecion_de_compactos
integer x = 462
integer y = 512
integer width = 1152
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExistePlanta(Integer(istr_mant.Argumento[1]), Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2] = data
ELSE
	RETURN 1
END IF
end event

type em_numero from editmask within w_reimprecion_de_compactos
integer x = 462
integer y = 656
integer width = 489
integer height = 92
integer taborder = 40
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
string mask = "########"
end type

event modified;istr_mant.argumento[4]	=	This.Text


end event

type st_3 from statictext within w_reimprecion_de_compactos
integer x = 174
integer y = 664
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Pallet"
boolean focusrectangle = false
end type

type st_5 from statictext within w_reimprecion_de_compactos
integer x = 82
integer y = 220
integer width = 1989
integer height = 700
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_16 from datawindow within w_reimprecion_de_compactos
boolean visible = false
integer x = 1006
integer y = 1020
integer width = 1202
integer height = 884
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod_gtin14"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type rb_1 from radiobutton within w_reimprecion_de_compactos
integer x = 439
integer y = 268
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Pallet"
boolean checked = true
boolean lefttext = true
end type

event clicked;IF This.Checked  THEN
	em_numero.Enabled = True
	em_1.Enabled = False
	em_1.Text = ''
END IF

em_numero.SetFocus()
end event

type rb_2 from radiobutton within w_reimprecion_de_compactos
integer x = 1152
integer y = 268
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Cajas"
boolean lefttext = true
end type

event clicked;IF This.Checked  THEN
	em_1.Enabled = True
	em_numero.Enabled = False
	em_numero.Text = ''
END IF

em_1.SetFocus()
end event

type dw_1 from datawindow within w_reimprecion_de_compactos
integer x = 178
integer y = 1028
integer width = 686
integer height = 400
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_palletfruta_caja"
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type em_1 from editmask within w_reimprecion_de_compactos
integer x = 1330
integer y = 644
integer width = 549
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[4]	=	This.Text


end event

type st_4 from statictext within w_reimprecion_de_compactos
integer x = 997
integer y = 664
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Nº Caja"
boolean focusrectangle = false
end type

type st_6 from statictext within w_reimprecion_de_compactos
integer x = 174
integer y = 800
integer width = 251
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Camara"
boolean focusrectangle = false
end type

type em_camara from editmask within w_reimprecion_de_compactos
integer x = 462
integer y = 788
integer width = 718
integer height = 92
integer taborder = 50
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
maskdatatype maskdatatype = stringmask!
string mask = "xxxxxxxxxxxxxxx"
double increment = 15
end type

event modified;istr_mant.argumento[4]	=	This.Text


end event

type ole_1 from olecustomcontrol within w_reimprecion_de_compactos
event click ( )
event dblclick ( )
event mousedown ( integer nbutton,  integer nshift,  long ocx_x,  long ocx_y )
event mousemove ( integer nbutton,  integer nshift,  long ocx_x,  long ocx_y )
event mouseup ( integer nbutton,  integer nshift,  long ocx_x,  long ocx_y )
event beforedraw ( )
boolean visible = false
integer x = 192
integer y = 1496
integer width = 389
integer height = 148
integer taborder = 130
boolean bringtotop = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "w_reimprecion_de_compactos.win"
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
0Aw_reimprecion_de_compactos.bin 
2F00001200e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd00000005fffffffe0000000400000006fffffffe00000007fffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff00000003000000000000000000000000000000000000000000000000000000006746665001ca688000000003000008000000000000500003004c004200430049004e0045004500530045004b000000590000000000000000000000000000000000000000000000000000000000000000000000000002001cffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000002e0000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000002001affffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000010000039900000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000101001a00000002000000010000000485382357404fca79c1bc00b25884d197000000006743f55001ca68806746665001ca6880000000000000000000000000fffffffe00000002000000030000000400000005000000060000000700000008000000090000000a0000000b0000000c0000000d0000000e0000000ffffffffe0000001100000012000000130000001400000015000000160000001700000018000000190000001a0000001b0000001c0000001d0000001e0000001ffffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
2Fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0079004d00720020006e00750074002d006d0069002000650069006c006500630073006e002000650065006b000000790000000000000000000000000000000000000800000008c9000003d3ffff0013000300ff00000000ffff000b520300098f910be3e39d11ce4b00aa00000151b8019000000001424420534d0c6c6568536c44206c000013670800000000000800320031000000330000000300030000000000140001000300030000000000000000000b000000080003000000ffffff00000003ff08000000000008003300330000003900000003000b0000000bffff00080000000000000030000800310000003a0042004200320033003a003a0042004200340031003a003a0053005300320033003a003a0053005300340008000000000020003a0031003a0032003a0033003a0034003a0031003a0032003a00330000003400000013000800000000000000000003000b0000000300000000000000ff00030005ffffff0000000051000000000003400b0000000500000000000000f00000000000033f0b000000080000000000000000000500000000000300000000000000000003000300000000000000ff00030003ffffff00000000000003000300000000000000000005000000000005000000000000000000000000000500000000000500000000000000000000000000030003000000000000000000050000000000034072c000000100000003000300000000000100000013000000000003000008ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff0000000000080000000003000b000000030000000000000000000b00ff00030003ffffffffffff00ff0003ff00ffffff030000080000000000000b00ff0003000bffffff0300000000000000020008000000000000000b0041001200410012000200080000000000000800000400030003000000ffffff00ff0003ff03ffffffffffff0000000bff02000800000000000200080000000000020008000000000006000800390000000000360000080000ff00030003ffffffffffff00ff0003ff03ffffff00000000ff00030008ffffff0000020003000000ffffff00ff0003ff08ffffff00000200080000000000020003000000ffffff00ff0003ff08ffffff0000020003000000ffffff00020008ff0000000000000b00000003000300000000000000000800000000030003000000000000000000030008000000000002000300000000000100ff00030003ffffffffffff00ff0003ff03ffffffffffff00ff0002ffff0003ff00ffffff0b00000803000000ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff00000100ff00030003ffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001020012ffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000010000003f5000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000800000008c9000003d3ffff0013000300ff00000000ffff000b520300098f910be3e39d11ce4b00aa00000151b8019000000001424420534d0c6c6568536c44206c000013670800000000000800320031000000330000000300030000000000140001000300030000000000000000000b000000080003000000ffffff00000003ff08000000000008003300330000003900000003000b0000000bffff00080000000000000030000800310000003a0042004200320033003a003a0042004200340031003a003a0053005300320033003a003a0053005300340008000000000020003a0031003a0032003a0033003a0034003a0031003a0032003a00330000003400000013000800000000005c0061004c006f0020006500700061007200690063006e00f300730020002000650061006800630020006d006f006c007000740065006400610020006f006f00630072007200630065006100740065006d0074006e002e00650000002000000003000b0000000300000000000000ff00030005ffffff0000000051000000000003400b000000
270500000000000000f00000000000033f0b000000080000000000000000000500000000000300000000000000000003000300000000000000ff00030003ffffff00000000000003000300000000000000000005000000000005000000000000000000000000000500000000000500000000000000000000000000030003000000000000000000050000000000034072c000000100000003000300000000000100000013000000000003000008ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff0000000000080000000003000b000000030000000000000000000b00ff00030003ffffffffffff00ff0003ff00ffffff030000080000000000000b00ff0003000bffffff0300000000000000020008000000000000000b0041001200410012000200080000000000000800000400030003000000ffffff00ff0003ff03ffffffffffff0000000bff02000800000000000200080000000000020008000000000006000800390000000000360000080000ff00030003ffffffffffff00ff0003ff03ffffff00000000ff00030008ffffff0000020003000000ffffff00ff0003ff08ffffff00000200080000000000020003000000ffffff00ff0003ff08ffffff0000020003000000ffffff00020008ff0000000000000b00000003000300000000000000000800000000030003000000000000000000030008000000000002000300000000000100ff00030003ffffffffffff00ff0003ff03ffffffffffff00ff0002ffff0003ff00ffffff0b00000803000000ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff00000100ff00030003ffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1Aw_reimprecion_de_compactos.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
