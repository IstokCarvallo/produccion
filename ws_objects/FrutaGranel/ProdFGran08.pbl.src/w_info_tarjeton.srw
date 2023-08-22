$PBExportHeader$w_info_tarjeton.srw
forward
global type w_info_tarjeton from w_para_informes
end type
type tit_peso from statictext within w_info_tarjeton
end type
type gb_3 from groupbox within w_info_tarjeton
end type
type dw_1 from datawindow within w_info_tarjeton
end type
type dw_2 from datawindow within w_info_tarjeton
end type
type ole_1 from olecustomcontrol within w_info_tarjeton
end type
end forward

global type w_info_tarjeton from w_para_informes
integer x = 14
integer y = 32
integer width = 2194
integer height = 972
string title = "IMPRESION DE ADHESIVOS"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
tit_peso tit_peso
gb_3 gb_3
dw_1 dw_1
dw_2 dw_2
ole_1 ole_1
end type
global w_info_tarjeton w_info_tarjeton

type variables
str_mant			istr_mant
uo_plantadesp	iuo_planta

Integer			ii_cantidad, ii_planta, ii_Tipo, ii_formato
Long				il_embaladora
String			is_abreviado
end variables

forward prototypes
protected function boolean wf_actualiza_db ()
public subroutine cargadatos ()
public function string carganombre (long al_tarjeta)
end prototypes

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_2.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
			
		dw_2.ResetUpdate()
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine cargadatos ();Integer	li_fila, respuesta, li_tipo

DO
	ii_planta	=	gstr_paramplanta.CodigoPlanta
	li_fila 		= 	dw_2.Retrieve(ii_planta)
	
	IF li_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)

	ELSEIF li_fila = 0 THEN
		dw_2.InsertRow(0)
		dw_2.Object.plde_codigo[1] 	=	ii_planta
		dw_2.Object.plde_nombre[1] 	=	iuo_planta.Nombre
	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(THIS)

IF Not IsNull(ii_cantidad)	 	THEN dw_2.Object.cota_cantid[1] 	=	ii_cantidad

dw_2.SetColumn("cota_cantid")
end subroutine

public function string carganombre (long al_tarjeta);String	ls_nombre

IF al_tarjeta = 0 OR IsNull(al_tarjeta) THEN Return ''

select pers_nombre + ' ' + pers_apepat + ' ' + pers_apemat
  into :ls_nombre
  from dbo.remupersonal
 where pers_nrotar = :al_tarjeta;
 
IF sqlca.SQLCode <> 0 THEN
	F_ErrorBaseDatos(sqlca, "Problemas con la carga del nombre del personal")
	RETURN ''
END IF

RETURN ls_nombre
end function

on w_info_tarjeton.create
int iCurrent
call super::create
this.tit_peso=create tit_peso
this.gb_3=create gb_3
this.dw_1=create dw_1
this.dw_2=create dw_2
this.ole_1=create ole_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tit_peso
this.Control[iCurrent+2]=this.gb_3
this.Control[iCurrent+3]=this.dw_1
this.Control[iCurrent+4]=this.dw_2
this.Control[iCurrent+5]=this.ole_1
end on

on w_info_tarjeton.destroy
call super::destroy
destroy(this.tit_peso)
destroy(this.gb_3)
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.ole_1)
end on

event resize;//
end event

event open;call super::open;Integer				li_fila, respuesta

iuo_planta	=	Create uo_plantadesp	
iuo_planta.Existe(gstr_paramplanta.CodigoPlanta, False, SQLCa)

ole_1.object.LicenseMe ("Mem: Exportadora Rio Blanco Santiago CL", 3, 1, "33E3AD94226C68E043BEF94763700EA9", 44)

dw_2.SetTransObject(sqlca)
dw_2.InsertRow(0)

ii_cantidad						=	0
dw_2.Object.cota_cantid[1]	=	0

CargaDatos()
end event

type pb_excel from w_para_informes`pb_excel within w_info_tarjeton
end type

type st_computador from w_para_informes`st_computador within w_info_tarjeton
end type

type st_usuario from w_para_informes`st_usuario within w_info_tarjeton
end type

type st_temporada from w_para_informes`st_temporada within w_info_tarjeton
end type

type p_logo from w_para_informes`p_logo within w_info_tarjeton
end type

type st_titulo from w_para_informes`st_titulo within w_info_tarjeton
integer width = 1545
string text = "Impresión de Tarjeton"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_tarjeton
string tag = "Imprimir Reporte"
integer x = 1865
integer y = 280
integer taborder = 100
end type

event pb_acepta::clicked;Long		fila, ll_cantidad, ll_actual, ll_ultimo, ll_tarja, ll_planta, ll_ultimoalter
Long		ll_control, ll_controlimp
String	ls_encabezado

SetPointer(HourGlass!)

dw_2.AcceptText()
dw_1.Reset()
dw_1.SetTransObject(sqlca)

ll_cantidad	=	dw_2.Object.cota_cantid[1]
ll_planta	=	dw_2.Object.plde_codigo[1]
ll_tarja		=	dw_2.Object.cota_correl[1]

IF ll_tarja = 0 OR IsNull(ll_tarja) THEN
	ll_tarja	=	0
END IF


IF ll_cantidad = 0 OR IsNull(ll_cantidad) THEN
	MessageBox("Error", "Debe ingresar cantidad de Tarjas a Imprimir")
	Return
	
END IF

FOR fila = ll_tarja TO ll_tarja + ll_cantidad - 1
	ll_actual							=	dw_1.InsertRow(0)
	dw_1.Object.folio[ll_actual]	=	fila
	
	ll_control ++
	CHOOSE CASE ll_control
		CASE 1
			dw_1.Object.Ole_1.Object.Text	=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.t_barra1.Text		=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.Ole_1.Visible 		= 	1
		CASE 2
			dw_1.Object.Ole_2.Object.Text	=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.t_barra2.Text		=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.Ole_2.Visible 		= 	1
			
		CASE 3
			dw_1.Object.Ole_3.Object.Text	=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.t_barra3.Text		=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.Ole_3.Visible 		= 	1
			
		CASE 4
			dw_1.Object.Ole_4.Object.Text	=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.t_barra4.Text		=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.Ole_4.Visible 		= 	1
			
		CASE 5
			dw_1.Object.Ole_5.Object.Text	=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.t_barra5.Text		=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.Ole_5.Visible 		= 	1
		
		CASE 6
			dw_1.Object.Ole_6.Object.Text	=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.t_barra6.Text		=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.Ole_6.Visible 		= 	1
			
		CASE 7
			dw_1.Object.Ole_7.Object.Text	=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.t_barra7.Text		=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.Ole_7.Visible 		= 	1
			
		CASE 8
			dw_1.Object.Ole_8.Object.Text	=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.t_barra8.Text		=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.Ole_8.Visible 		= 	1
			
		CASE 9
			dw_1.Object.Ole_9.Object.Text	=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.t_barra9.Text		=	'.F.' + String(ll_planta, '0000') + String(dw_1.Object.folio[ll_actual], '00000000')
			dw_1.Object.Ole_9.Visible 		= 	1
			ll_control							=	0
			ll_controlimp 						= 	dw_1.Print()
			
			dw_1.Reset()
			dw_1.Object.Ole_1.Visible 		= 	0
			dw_1.Object.Ole_2.Visible 		= 	0
			dw_1.Object.Ole_3.Visible 		= 	0
			dw_1.Object.Ole_4.Visible 		= 	0
			dw_1.Object.Ole_5.Visible 		= 	0
			dw_1.Object.Ole_6.Visible 		= 	0
			dw_1.Object.Ole_7.Visible 		= 	0
			dw_1.Object.Ole_8.Visible 		= 	0
			dw_1.Object.Ole_9.Visible 		= 	0
			
	END CHOOSE
	
	IF ll_controlimp = -1 THEN
		Exit
	END IF
NEXT

IF dw_1.RowCount() > 0 THEN
	ll_control							=	0
	ll_controlimp 						= 	dw_1.Print()
	dw_1.Reset()
END IF

IF ll_controlimp = -1 THEN
	MessageBox("Error", "No se pudo realizar la impresión")
ELSE
	dw_2.Object.cota_correl[1]	=	fila
	dw_2.Object.cota_cantid[1]	=	0
	IF wf_actualiza_db() THEN
		w_main.SetMicroHelp("Información Grabada.")
		
	ELSE
		w_main.SetMicroHelp("No se puede Grabar información.")
		Message.DoubleParm = -1
		RETURN
		
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_tarjeton
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 1865
integer y = 540
integer taborder = 110
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type tit_peso from statictext within w_info_tarjeton
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

type gb_3 from groupbox within w_info_tarjeton
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

type dw_1 from datawindow within w_info_tarjeton
boolean visible = false
integer x = 32
integer y = 548
integer width = 800
integer height = 276
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_tarjeton"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_info_tarjeton
integer x = 247
integer y = 400
integer width = 1504
integer height = 340
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_correltarjetones"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Integer	li_fila, respuesta, ls_nula
String		ls_columna

SetNull(ls_nula)

ls_columna	=	dwo.name

CHOOSE CASE ls_columna
	CASE "cota_cantid"
		
END CHOOSE
end event

event itemerror;Return 1
end event

type ole_1 from olecustomcontrol within w_info_tarjeton
event click ( )
event dblclick ( )
event mousedown ( integer nbutton,  integer nshift,  long ocx_x,  long ocx_y )
event mousemove ( integer nbutton,  integer nshift,  long ocx_x,  long ocx_y )
event mouseup ( integer nbutton,  integer nshift,  long ocx_x,  long ocx_y )
event beforedraw ( )
boolean visible = false
integer x = 1189
integer y = 740
integer width = 389
integer height = 200
integer taborder = 130
boolean bringtotop = true
boolean border = false
boolean focusrectangle = false
string binarykey = "w_info_tarjeton.win"
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
0Fw_info_tarjeton.bin 
2C00001400e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd00000006fffffffe000000040000000500000007fffffffe00000008fffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff0000000300000000000000000000000000000000000000000000000000000000435947d001d531d400000003000008400000000000500003004c004200430049004e0045004500530045004b000000590000000000000000000000000000000000000000000000000000000000000000000000000002001cffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000002e0000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000002001affffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000001000003f100000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000101001a00000002000000010000000485382357404fca79c1bc00b25884d197000000004358f9b001d531d4435920c001d531d4000000000000000000000000fffffffe00000002000000030000000400000005000000060000000700000008000000090000000a0000000b0000000c0000000d0000000e0000000f00000010fffffffe00000012000000130000001400000015000000160000001700000018000000190000001a0000001b0000001c0000001d0000001e0000001f00000020fffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
2Fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0079004d00720020006e00750074002d006d0069002000650069006c006500630073006e002000650065006b000000790000000000000000000000000000000000000900000008c90000052bffff0013000300ff00000000ffff000b520300098f910be3e39d11ce4b00aa00000151b8019000000001424420534d0c6c6568536c44206c000013670800000000000e007300610061006400640073000300000000000000140003000300000000000100000003000b0000000800000000000000ff00030003ffffff000000000800080033000000390033000300000000000000ff000b0000000bff0000080008000000000030004200310032003a003a0042004200330034003a003a0042005300310032003a003a0053005300330034003a0000005300200008003100000032003a0033003a0034003a0031003a0032003a0033003a0034003a001300000000000000520008004c0000002000610070006f007200650063006100f300690020006e0065007300630020006d006f006c007000740065002000f3006f00630072007200630065006100740065006d0074006e002e00650000002000000003000b0000000300000000000000ff00030005ffffff0000000059400000000003400b0000000500000000000000f00000000000033f0b000000080000000000000000000500000000000300000000000000520003000300000000000000ff00030003ffffff00000000000003000300000000000000000005000000000005000000000000000000000000000500000000000500000000000000000000000000030003000000000000000000050000000000034072c000000100000003000300000000000100000013000000000003000009ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff0000000000090000000003000b000000030000000000000000000b00ff00030003ffffffffffff00ff0003ff00ffffff030000090000000000000b00ff0003000bffffff0300000000000000020008000000000000000b0041001200410012000200080000000000000900000400030003000000ffffff00ff0003ff03ffffffffffff0000000bff02000800000000000200080000000000020008000000000006000800390000000000360000090000ff00030003ffffffffffff00ff0003ff03ffffff00000000ff00030008ffffff0000020003000000ffffff00ff0003ff08ffffff00000200080000000000020003000000ffffff00ff0003ff08ffffff0000020003000000ffffff00020008ff0000000000000b00000003000300000000000000000900000000030003000000000000000000030008000000000002000300000000000100ff00030003ffffffffffff00ff0003ff03ffffffffffff00ff0002ffff0003ff00ffffff0b00000903000000ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff00000100ff00030003ffffff000000000000000000000000000000000000000000000900000008c90000052bffff0013000300ff00000000ffff000b520300098f910be3e39d11ce4b00aa00000151b8019000000001424420534d0c6c6568536c44206c000013670800000000000e007300610061006400640073000300000000000000140003000300000000000100000003000b0000000800000000000000ff00030003ffffff000000000800080033000000390033000300000000000000ff000b0000000bff0000080008000000000030004200310032003a003a0042004200330034003a003a0042005300310032003a003a0053005300330034003a0000005300200008003100000032003a0033003a0034003a0031003a0032003a0033003a0034003a001300000000000000520008004c0000002000610070006f007200650063006100f300690020006e0065007300630020006d006f006c007000740065002000f3006f00630072007200630065006100740065006d0074006e002e00650000002000000003000b0000000300000000000000ff00030005ffffff0000000059400000000003400b0000000500000000000000f00000000000033f0b00000008000000000000000000050000000000030000000000000052000300006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001020012ffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000011000003f1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
28000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000300000000000000ff00030003ffffff00000000000003000300000000000000000005000000000005000000000000000000000000000500000000000500000000000000000000000000030003000000000000000000050000000000034072c000000100000003000300000000000100000013000000000003000009ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff0000000000090000000003000b000000030000000000000000000b00ff00030003ffffffffffff00ff0003ff00ffffff030000090000000000000b00ff0003000bffffff0300000000000000020008000000000000000b0041001200410012000200080000000000000900000400030003000000ffffff00ff0003ff03ffffffffffff0000000bff02000800000000000200080000000000020008000000000006000800390000000000360000090000ff00030003ffffffffffff00ff0003ff03ffffff00000000ff00030008ffffff0000020003000000ffffff00ff0003ff08ffffff00000200080000000000020003000000ffffff00ff0003ff08ffffff0000020003000000ffffff00020008ff0000000000000b00000003000300000000000000000900000000030003000000000000000000030008000000000002000300000000000100ff00030003ffffffffffff00ff0003ff03ffffffffffff00ff0002ffff0003ff00ffffff0b00000903000000ffffff00ff0003ff03ffffffffffff00ff0003ff03ffffff00000100ff00030003ffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1Fw_info_tarjeton.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
