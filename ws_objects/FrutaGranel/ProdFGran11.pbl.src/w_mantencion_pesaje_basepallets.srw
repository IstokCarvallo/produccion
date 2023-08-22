$PBExportHeader$w_mantencion_pesaje_basepallets.srw
forward
global type w_mantencion_pesaje_basepallets from w_mant_directo
end type
type dw_estadistica from datawindow within w_mantencion_pesaje_basepallets
end type
type st_1 from statictext within w_mantencion_pesaje_basepallets
end type
type st_2 from statictext within w_mantencion_pesaje_basepallets
end type
type sle_tarja from singlelineedit within w_mantencion_pesaje_basepallets
end type
type ole_puerta from olecustomcontrol within w_mantencion_pesaje_basepallets
end type
type em_kilos from singlelineedit within w_mantencion_pesaje_basepallets
end type
type dw_destinatarios from datawindow within w_mantencion_pesaje_basepallets
end type
type str_pesaje from structure within w_mantencion_pesaje_basepallets
end type
end forward

type str_pesaje from structure
	datetime		fechahora[]
	decimal { 4 }		pesaje[]
	decimal { 4 }		total
	boolean		agrega
	boolean		modifica
	str_puertacomm		puerta
	datawindow		dw
	string		argum[]
end type

global type w_mantencion_pesaje_basepallets from w_mant_directo
integer width = 4681
integer height = 2656
string title = "VENTANA DE PESAJE DE PALLETS GRANEL"
boolean center = true
event ue_validapassword ( )
dw_estadistica dw_estadistica
st_1 st_1
st_2 st_2
sle_tarja sle_tarja
ole_puerta ole_puerta
em_kilos em_kilos
dw_destinatarios dw_destinatarios
end type
global w_mantencion_pesaje_basepallets w_mantencion_pesaje_basepallets

type variables
str_puertacomm		istr_puertacomm
Double				id_kilos, id_kiloant
Integer				ii_count

uo_Calicosechero	iuo_Peso
uo_productores		iuo_prod
end variables

forward prototypes
public subroutine cargakilos ()
public function boolean enviacorreo ()
end prototypes

event ue_validapassword();str_mant	lstr_mant

lstr_mant.Argumento[1]	=	"Granel"
lstr_mant.Argumento[2]	=	gstr_paramplanta.Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public subroutine cargakilos ();Double	ld_peso
Integer	li_largo
Dec{2}	ld_Base, ld_envase
Dec{6}	ld_Total, ld_Porcentaje, ld_Unitario

SetPointer(HourGlass!)

IF dw_1.RowCount() < 1 THEN RETURN

IF gstr_paramplanta.aplicaporc = 1 THEN 
	gstr_paramplanta.porcentaje	=	iuo_Prod.Porcentaje(dw_1.Object.prod_codigo[1], &
																		  dw_1.Object.lote_espcod[1], False, SQLCa)
	
	ld_Envase							=	iuo_Peso.PesoEnvase(dw_1.Object.clie_codigo[1], &
																		  dw_1.Object.plde_codigo[1], &
																		  dw_1.Object.bins_numero[1], 0, False, Sqlca)
	ld_Base								=	iuo_Peso.PesoEnvase(dw_1.Object.clie_codigo[1], &
																		  dw_1.Object.plde_codigo[1], &
																		  dw_1.Object.mfgp_tibapa[1], 1, False, Sqlca)

	ld_Porcentaje						=	(((id_kilos - ld_Base) - (ld_Envase * dw_1.RowCount())) * gstr_paramplanta.porcentaje) / 100
	ld_Total								=	id_kilos - ld_Porcentaje
	ld_Unitario							=	ld_Total / dw_1.RowCount()
	
ELSE
	ld_Total		=	id_kilos
	ld_Unitario	=	id_kilos / dw_1.RowCount()
	
END IF

ld_peso	=	id_kilos / dw_1.RowCount()

FOR li_largo = 1 TO dw_1.RowCount()
	dw_1.Object.mfgp_pesore[li_largo]	=	ld_Unitario
	dw_1.Object.mfgp_valref[li_largo]	=	ld_Total	
	
	dw_1.Object.mfgp_pesori[li_largo]	=	ld_peso
	dw_1.Object.mfgp_valori[li_largo]	=	id_kilos
	
	dw_1.Object.mfgp_porcen[li_largo]	=	gstr_paramplanta.porcentaje
	dw_1.Object.mfgp_comnom[li_largo]	=	gstr_us.computador
NEXT

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	dw_1.Reset()
	dw_1.InsertRow(0)
	sle_tarja.Text	=	''
	EnviaCorreo()
	dw_estadistica.Retrieve()
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end subroutine

public function boolean enviacorreo ();Long 		ll_resultado, li_filas
Integer	li_tipo
String 	sErrorMsg, ls_para, ls_cc, ls_cco, ls_persona, ls_fecha, ls_mensaje, ls_titulo, ls_nombre, ls_Computador

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", "ComputerName", RegString!, ls_Computador)


li_filas	=	dw_destinatarios.Retrieve(2)

IF li_filas < 1 THEN
	Messagebox("Error", "No Existen destinatarios para el correo", StopSign!)
	Return False
	
END IF

FOR li_filas = 1 TO dw_destinatarios.RowCount()
	li_tipo	=	dw_destinatarios.Object.sppc_tipcor[li_filas]
	
	CHOOSE CASE li_tipo
		CASE 1
			ls_para	=	ls_para 	+ "<" + dw_destinatarios.Object.sppc_correo[li_filas] + ">;"
			
		CASE 2
			ls_cc		=	ls_cc 	+ "<" + dw_destinatarios.Object.sppc_correo[li_filas] + ">;"
			
		CASE 3
			ls_cco	=	ls_cco 	+ "<" + dw_destinatarios.Object.sppc_correo[li_filas] + ">;"
			
	END CHOOSE
NEXT

ls_titulo	=	"Aviso Modificación de Kilos"
ls_mensaje	=	"Se ha modificado la información de kilos de la tarja " + istr_mant.argumento[2]		+ 	&
					", cambiando de " + String(id_kiloant) + " Kgs Brutos a " + String(id_kilos) + " Kgs Brutos.~r~n" 	+ 	&
					"El nombre del computador donde se modificaron los datos es " +  ls_Computador + ", "	+ 	&
					"con el usuario " + gstr_us.nombre


//ll_resultado = send_mail("smtp.rioblanco.cl","<FrutaGranel@RioBlanco.cl>", ls_para, ls_cc, ls_cco, ls_titulo, ls_mensaje, "", sErrorMsg)
	
IF (ll_resultado < 0) THEN
	Messagebox("Error No" + String(ll_resultado), "El problema con el envio de correo es el siguiente : " + sErrorMsg)
	Return False
END IF

Return True
	
end function

on w_mantencion_pesaje_basepallets.create
int iCurrent
call super::create
this.dw_estadistica=create dw_estadistica
this.st_1=create st_1
this.st_2=create st_2
this.sle_tarja=create sle_tarja
this.ole_puerta=create ole_puerta
this.em_kilos=create em_kilos
this.dw_destinatarios=create dw_destinatarios
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_estadistica
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.sle_tarja
this.Control[iCurrent+5]=this.ole_puerta
this.Control[iCurrent+6]=this.em_kilos
this.Control[iCurrent+7]=this.dw_destinatarios
end on

on w_mantencion_pesaje_basepallets.destroy
call super::destroy
destroy(this.dw_estadistica)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.sle_tarja)
destroy(this.ole_puerta)
destroy(this.em_kilos)
destroy(this.dw_destinatarios)
end on

event open;String	ls_parametros
Integer	li_resultado
Boolean	ib_OCX	=	True

x				= 	0
y				= 	0
This.Height	=	2520	

This.ParentWindow().ToolBarVisible	=	True
im_menu										= 	m_principal
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon										=	Gstr_apl.Icono

dw_destinatarios.SetTransObject(sqlca)

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
							
dw_1.InsertRow(0)
dw_estadistica.SetTRansObject(sqlca)
dw_estadistica.Retrieve()

iuo_prod				=	Create uo_productores

li_resultado 		=	ConfiguracionPuerta(istr_puertacomm)

IF li_resultado 	= 	0 THEN
	ls_parametros	=	String(istr_puertacomm.Baudios)+","+&
							istr_puertacomm.Paridad+","+&
							String(istr_puertacomm.Data)+","+&
							String(istr_puertacomm.Parada)
			
	IF NOT ib_OCX THEN
		MessageBox("Conexión Romana","No está instalado el OCX para conexión con Romana")
	ELSE
		IF Ole_puerta.object.PortOpen THEN Ole_puerta.object.PortOpen = False
		Ole_puerta.object.settings	=	ls_parametros
		Ole_puerta.object.PortOpen	= 	True	
		Timer(0.2)
	END IF
END IF

IF istr_puertacomm.hidrocooler = 1 THEN
	st_2.Text	=	"Pesaje de Pallet de Granel - HidroCooler"
ELSE
	st_2.Text	=	"Pesaje de Pallet de Granel - Frigorifico"
END IF
	
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta
iuo_Peso								=	Create uo_Calicosechero

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

sle_tarja.SetFocus()

IF NOT IsNull(gstr_paramplanta.Password) AND Trim(gstr_paramplanta.Password) <> '' THEN
	PostEvent("ue_validapassword")
END IF
end event

event timer;call super::timer;Integer	li_factor, li_posini, li_LarBuf
String 	ls_string

Ole_Puerta.Object.inputlen =	Integer(istr_puertacomm.LargoLectura)

li_LarBuf =	Ole_Puerta.Object.InBufferCount

IF li_LarBuf > 0 THEN
	ls_string =  Ole_Puerta.Object.input
END IF

li_posini = Pos(ls_string,istr_puertacomm.CadenaInicio) + Len(istr_puertacomm.CadenaInicio)

IF Len(Mid(ls_string,li_posini)) < istr_puertacomm.LargoCadena THEN RETURN
	
IF IsNumber(Mid(ls_string,li_posini,istr_puertacomm.LargoCadena)) THEN
	id_kilos	=	Dec(Mid(ls_string,li_posini,istr_puertacomm.LargoCadena))
	IF istr_puertacomm.Decimales > 0 THEN
		li_factor	= 10 ^ istr_puertacomm.Decimales
		id_kilos		= Round(id_kilos / li_factor, istr_puertacomm.Decimales)
	END IF
	em_kilos.Text	=	String(id_kilos)
END IF

IF id_kiloant = id_kilos THEN
	ii_count ++
ELSE
	ii_count = 0
	id_kiloant = id_kilos
END IF

IF ii_count = 20 THEN
	CargaKilos()
END IF
end event

event ue_recuperadatos;Long	ll_fila_e, respuesta

w_main.SetMicroHelp("Recuperando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

DO
	IF dw_1.Retrieve(Integer(istr_mant.argumento[1]),&
						  Long(istr_mant.argumento[2]),&
						  gstr_paramplanta.codigoplanta) = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		IF dw_1.RowCount() < 1 THEN
			dw_1.InsertRow(0)
			Message.DoubleParm = -1
			RETURN
		END IF
		IF dw_1.RowCount() <> dw_1.Object.fgmb_canbul[1] THEN
			MessageBox("Error de consistencia", "Bultos informados en la tarja no se corresponde~r~n"+&
															"con el total de bultos generados.", Exclamation!)
			dw_1.Reset()
			dw_1.InsertRow(0)
			Message.DoubleParm = -1
			RETURN
		ELSEIF dw_1.Object.mfgp_valref[1] <> 0 THEN
			em_kilos.text	=	String(dw_1.Object.mfgp_valref[1])
			id_kiloant		=	dw_1.Object.mfgp_valref[1]
		END IF
		dw_1.SetRow(1)
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event resize;call super::resize;dw_1.Resize(This.WorkSpaceWidth() - 735, This.WorkSpaceHeight() - dw_1.y - dw_estadistica.Height - 100)

st_encabe.Width	=	dw_1.Width
sle_tarja.x			=	st_encabe.x + 40
em_kilos.x			=	sle_tarja.x + sle_tarja.Width + 40
st_1.x					=	st_encabe.x 
st_2.x					=	st_encabe.x + 20 
st_1.Width			=	dw_1.Width
st_2.Width			=	dw_1.Width - 30
dw_Estadistica.y	=	dw_1.y + dw_1.Height + 20
end event

type st_encabe from w_mant_directo`st_encabe within w_mantencion_pesaje_basepallets
integer x = 50
integer y = 260
integer width = 3918
integer height = 444
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mantencion_pesaje_basepallets
boolean visible = false
integer x = 4174
integer y = 412
integer taborder = 40
end type

type pb_lectura from w_mant_directo`pb_lectura within w_mantencion_pesaje_basepallets
integer x = 4174
integer y = 116
integer taborder = 30
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
end type

event pb_lectura::clicked;CargaKilos()
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mantencion_pesaje_basepallets
boolean visible = false
integer x = 4174
integer y = 772
integer taborder = 60
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mantencion_pesaje_basepallets
boolean visible = false
integer x = 4174
integer y = 592
integer taborder = 50
end type

type pb_salir from w_mant_directo`pb_salir within w_mantencion_pesaje_basepallets
integer x = 4174
integer y = 1516
integer taborder = 90
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mantencion_pesaje_basepallets
boolean visible = false
integer x = 4174
integer y = 1132
integer taborder = 80
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mantencion_pesaje_basepallets
boolean visible = false
integer x = 4174
integer y = 952
integer taborder = 70
end type

type dw_1 from w_mant_directo`dw_1 within w_mantencion_pesaje_basepallets
integer x = 55
integer y = 708
integer width = 3918
integer height = 988
integer taborder = 0
string dataobject = "dw_mues_movtogranpesa_grande"
boolean vscrollbar = false
boolean livescroll = false
end type

type dw_estadistica from datawindow within w_mantencion_pesaje_basepallets
integer x = 224
integer y = 1708
integer width = 3643
integer height = 676
integer taborder = 110
boolean bringtotop = true
boolean titlebar = true
string title = "Recepciones Pendientes"
string dataobject = "dw_recepciones_pendientes_proc"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_mantencion_pesaje_basepallets
integer x = 50
integer y = 32
integer width = 3918
integer height = 228
boolean bringtotop = true
integer textsize = -40
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_mantencion_pesaje_basepallets
integer x = 64
integer y = 84
integer width = 3890
integer height = 124
boolean bringtotop = true
integer textsize = -20
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Pesaje de Pallet de Granel - Frigorifico"
alignment alignment = center!
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type sle_tarja from singlelineedit within w_mantencion_pesaje_basepallets
integer x = 82
integer y = 284
integer width = 1975
integer height = 380
integer taborder = 10
boolean bringtotop = true
integer textsize = -60
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

event modified;Integer	li_largo

li_largo	=	Len(This.Text)
IF li_largo < 6 OR li_largo > 8 OR Not IsNumber(This.Text) THEN
	MessageBox("Error de Consistencia", "La tarja ingresada no es valida", Exclamation!)
	This.Text				=	''
	This.SetFocus()
	Return
END IF

istr_mant.argumento[2]	=	This.Text
li_largo						=	li_largo - 5
istr_mant.argumento[1]	=	Left(istr_mant.argumento[2], li_largo)

Parent.TriggerEvent("ue_recuperadatos")
end event

type ole_puerta from olecustomcontrol within w_mantencion_pesaje_basepallets
event oncomm ( )
boolean visible = false
integer x = 1998
integer y = 400
integer width = 174
integer height = 152
integer taborder = 100
boolean bringtotop = true
borderstyle borderstyle = stylelowered!
long backcolor = 8388608
boolean focusrectangle = false
string binarykey = "w_mantencion_pesaje_basepallets.win"
integer textsize = -40
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 15793151
end type

type em_kilos from singlelineedit within w_mantencion_pesaje_basepallets
integer x = 2103
integer y = 284
integer width = 1819
integer height = 380
integer taborder = 20
boolean bringtotop = true
integer textsize = -60
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 15793151
long backcolor = 8388608
boolean autohscroll = false
integer limit = 9
borderstyle borderstyle = stylelowered!
end type

event modified;id_kilos	=	Dec(This.Text)
end event

type dw_destinatarios from datawindow within w_mantencion_pesaje_basepallets
boolean visible = false
integer x = 59
integer y = 84
integer width = 87
integer height = 84
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_paramcorreo_filtro"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
0Fw_mantencion_pesaje_basepallets.bin 
2B00000600e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe00000006000000000000000000000001000000010000000000001000fffffffe00000000fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1Fw_mantencion_pesaje_basepallets.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
