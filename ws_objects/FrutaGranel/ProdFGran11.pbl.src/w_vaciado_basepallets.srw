$PBExportHeader$w_vaciado_basepallets.srw
forward
global type w_vaciado_basepallets from w_mant_directo
end type
type ole_puerta from olecustomcontrol within w_vaciado_basepallets
end type
type dw_estadistica from datawindow within w_vaciado_basepallets
end type
type dw_encab from uo_dw within w_vaciado_basepallets
end type
type dw_deta from uo_dw within w_vaciado_basepallets
end type
type sle_tarja from singlelineedit within w_vaciado_basepallets
end type
type em_grua from singlelineedit within w_vaciado_basepallets
end type
type em_kilos from singlelineedit within w_vaciado_basepallets
end type
type str_pesaje from structure within w_vaciado_basepallets
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

global type w_vaciado_basepallets from w_mant_directo
integer width = 4416
integer height = 2484
string title = "VENTANA DE PESAJE DE VACIADO GRANEL"
boolean maxbox = false
boolean resizable = false
ole_puerta ole_puerta
dw_estadistica dw_estadistica
dw_encab dw_encab
dw_deta dw_deta
sle_tarja sle_tarja
em_grua em_grua
em_kilos em_kilos
end type
global w_vaciado_basepallets w_vaciado_basepallets

type variables
uo_grua						iuo_grua
uo_bins						iuo_bins

str_puertacomm				istr_puertacomm
uo_calicosechero			Iuo_calicosechero
uo_validacionesvaciado	iuo_valida

Double						id_kilos, id_kiloant
Integer						ii_count

Integer						ii_cliente, ii_tipo, ii_orden, ii_turno, ii_linea, ii_grua
Long							ii_planta
Date							id_fecha

end variables

forward prototypes
public subroutine cargakilos ()
protected function boolean wf_actualiza_db ()
public function boolean validatarja (long al_nrotarja)
end prototypes

public subroutine cargakilos ();Integer	li_fila
Double	ld_kilos, ld_TaraBasePallet

SetPointer(HourGlass!)

IF dw_1.RowCount() < 1 THEN RETURN

IF dw_encab.GetItemStatus(1, 0, Primary!) = New! THEN
	dw_encab.Object.clie_codigo[1]	=	ii_cliente
	dw_encab.Object.plde_codigo[1]	=	ii_planta
	dw_encab.Object.opve_estado[1]	=	'1'
	dw_encab.Object.opve_fecvac[1]	=	Date(f_fechahora())
	dw_encab.Object.opve_turno[1]		=	ii_turno
	dw_encab.Object.line_codigo[1]	=	ii_linea
	dw_encab.Object.orpr_tipord[1]	=	ii_tipo
	dw_encab.Object.orpr_numero[1]	=	ii_orden
END IF

iuo_bins.Existe(ii_cliente, ii_planta, dw_1.Object.fgmb_tibapa[1], SQLca, True)
ld_TaraBasePallet							=	iuo_bins.cale_pesoen
iuo_bins.Existe(ii_cliente, ii_planta, dw_1.Object.bins_numero[1], SQLca, True)
ld_TaraBasePallet							=	ld_TaraBasePallet + ( iuo_bins.cale_pesoen * dw_1.Object.fgmb_canbul[1] )

ld_kilos										=	Dec(em_kilos.Text)
li_fila										=	dw_deta.InsertRow(0)
dw_deta.Object.opve_nrtar1[li_fila]	=	Long(istr_mant.argumento[2])
dw_deta.Object.orpr_tipord[li_fila]	=	ii_tipo
dw_deta.Object.orpr_numero[li_fila]	=	ii_orden	
dw_deta.Object.plde_codigo[li_fila]	=	ii_planta
dw_deta.Object.clie_codigo[li_fila]	=	ii_cliente
dw_deta.Object.opvd_horava[li_fila]	=	Time(f_fechahora())
dw_deta.Object.opve_turno[li_fila]	=	ii_turno
dw_deta.Object.lote_pltcod[li_fila] =	ii_planta
dw_deta.Object.lote_espcod[li_fila] =	dw_1.Object.lote_espcod[1]
dw_deta.Object.lote_codigo[li_fila]	=	dw_1.Object.lote_codigo[1]
dw_deta.Object.opve_fecvac[li_fila]	=	Date(f_fechahora())
dw_deta.Object.enva_tipoen[li_fila]	= 	dw_1.Object.enva_tipoen[1]
dw_deta.Object.enva_codigo[li_fila]	= 	dw_1.Object.enva_codigo[1]
dw_deta.Object.cale_calida[li_fila]	= 	dw_1.Object.cale_calida[1]
dw_deta.Object.opvd_canbul[li_fila] = 	dw_1.Object.fgmb_canbul[1]
dw_deta.Object.opvd_pesobr[li_fila] = 	ld_kilos
dw_deta.Object.opvd_pesone[li_fila] = 	ld_kilos - ld_TaraBasePallet - iuo_grua.tagr_kiltar
dw_deta.Object.opvd_kilori[li_fila]	=	((dw_1.Object.mfgp_pesore[1] * dw_1.Object.fgmb_canbul[1]) - ld_TaraBasePallet )
dw_deta.Object.opvd_kilpro[li_fila]	=	((ld_kilos - ld_TaraBasePallet - iuo_grua.tagr_kiltar) / dw_1.Object.fgmb_canbul[1])

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	dw_1.Reset()
	dw_1.InsertRow(0)
	sle_tarja.Text	=	''
	em_grua.Text	=	''
	dw_estadistica.Retrieve()
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end subroutine

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_encab.Update(True, False) = 1 THEN 
	IF dw_deta.Update(True, False) = 1 THEN
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
			lb_Retorno	=	False
		ELSE
			lb_Retorno	=	True
			dw_1.ResetUpdate()
		END IF
	ELSE
		RollBack;
		IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean validatarja (long al_nrotarja);Integer 	li_Cliente, li_Planta, li_lote, li_Especie, li_Envase, li_TipoEnva, li_TipOrden, li_Numero, li_fila
Long		ll_Bins, ll_cantidad
String	ls_Calidad

iuo_valida	=	Create uo_validacionesvaciado

li_Cliente 	= 	ii_cliente
li_Planta 	= 	ii_planta
li_TipOrden	=	ii_tipo
li_Numero	=	ii_orden

IF NOT iuo_valida.binsduplicados(li_Cliente, li_Planta, al_nrotarja, TRUE, sqlca) THEN
	Return True		
END IF

ll_bins		=	iuo_valida.Bins
li_lote		=	iuo_valida.Lote
li_especie	=	iuo_valida.Especie

IF Not manbin_especie(ii_planta, li_especie, True, sqlca) THEN
	Return True
END IF

IF NOT iuo_valida.ValidaLote(li_Planta, li_Cliente, li_TipOrden, li_Numero, li_lote, TRUE, sqlca) THEN
	Return True		
END IF
		
IF NOT iuo_valida.cargabins(li_Cliente, li_Planta, ll_Bins, TRUE, sqlca) THEN
	Return True		
END IF

li_TipoEnva	=	iuo_valida.TipoEnva
li_Envase	=	iuo_valida.Envase
ls_Calidad 	=	iuo_valida.Calidad

Destroy iuo_valida;

RETURN False
end function

on w_vaciado_basepallets.create
int iCurrent
call super::create
this.ole_puerta=create ole_puerta
this.dw_estadistica=create dw_estadistica
this.dw_encab=create dw_encab
this.dw_deta=create dw_deta
this.sle_tarja=create sle_tarja
this.em_grua=create em_grua
this.em_kilos=create em_kilos
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.ole_puerta
this.Control[iCurrent+2]=this.dw_estadistica
this.Control[iCurrent+3]=this.dw_encab
this.Control[iCurrent+4]=this.dw_deta
this.Control[iCurrent+5]=this.sle_tarja
this.Control[iCurrent+6]=this.em_grua
this.Control[iCurrent+7]=this.em_kilos
end on

on w_vaciado_basepallets.destroy
call super::destroy
destroy(this.ole_puerta)
destroy(this.dw_estadistica)
destroy(this.dw_encab)
destroy(this.dw_deta)
destroy(this.sle_tarja)
destroy(this.em_grua)
destroy(this.em_kilos)
end on

event open;String	ls_parametros
Integer	li_resultado

x												= 	0
y												= 	0

This.ParentWindow().ToolBarVisible	=	True
im_menu										= 	m_principal
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono

iuo_grua										=	Create uo_grua
iuo_bins										=	Create uo_bins

dw_1.SetTransObject(sqlca)
dw_deta.SetTransObject(sqlca)
dw_encab.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")

dw_1.InsertRow(0)
dw_estadistica.SetTRansObject(sqlca)
dw_estadistica.Retrieve()

Boolean	ib_OCX	=	True
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

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							
sle_tarja.SetFocus()
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

IF ii_count = 20 AND iuo_grua.tagr_kiltar > 0 THEN
	CargaKilos()
END IF
end event

event ue_recuperadatos;Long		ll_fila_e, respuesta

w_main.SetMicroHelp("Recuperando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

DO
	IF dw_1.Retrieve(Integer(istr_mant.argumento[1]),&
						  Long(istr_mant.argumento[2]), &
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
		END IF
		
		ii_planta	=	dw_1.Object.plde_codigo[1]
		ii_tipo		=	dw_1.Object.orpr_tipord[1]
		ii_orden		=	dw_1.Object.orpr_numero[1]
		id_fecha		=	dw_1.Object.orpr_fecpro[1]
		ii_turno		=	dw_1.Object.orpr_nrotur[1]
		ii_cliente	=	dw_1.Object.clie_codigo[1]
		ii_linea		=	dw_1.Object.line_codigo[1]
		
		IF ValidaTarja(Long(istr_mant.argumento[2])) THEN
			dw_1.Reset()
			dw_1.InsertRow(0)
			Message.DoubleParm = -1
			RETURN
		END IF
		
		IF id_fecha <> Date(F_FechaHora()) THEN
			MessageBox("Error", "La base pallet ingresada no pertenece a un proceso del día", StopSign!)
			dw_1.Reset()
			dw_1.InsertRow(0)
			Message.DoubleParm = -1
			RETURN
		END IF
		
		DO
			IF	dw_encab.retrieve(ii_planta, ii_tipo, ii_orden, id_fecha, ii_turno, ii_cliente) = -1 OR &
				dw_deta.retrieve(ii_planta, ii_tipo, ii_orden, id_fecha, ii_turno, ii_cliente) = -1 THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				IF dw_encab.RowCount() < 1 THEN
					dw_encab.InsertRow(0)
				END IF
			END IF
		LOOP WHILE respuesta = 1
		
		IF respuesta = 2 THEN Close(This)
		dw_1.SetRow(1)
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event resize;//
end event

type st_encabe from w_mant_directo`st_encabe within w_vaciado_basepallets
integer y = 32
integer width = 4005
integer height = 432
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_vaciado_basepallets
boolean visible = false
integer x = 4174
integer y = 412
integer taborder = 50
long backcolor = 553648127
end type

type pb_lectura from w_mant_directo`pb_lectura within w_vaciado_basepallets
integer x = 4174
integer y = 116
integer taborder = 40
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
long backcolor = 553648127
end type

event pb_lectura::clicked;CargaKilos()
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_vaciado_basepallets
boolean visible = false
integer x = 4174
integer y = 772
integer taborder = 70
long backcolor = 553648127
end type

type pb_insertar from w_mant_directo`pb_insertar within w_vaciado_basepallets
boolean visible = false
integer x = 4174
integer y = 592
integer taborder = 60
long backcolor = 553648127
end type

type pb_salir from w_mant_directo`pb_salir within w_vaciado_basepallets
integer x = 4174
integer y = 1516
integer taborder = 100
long backcolor = 553648127
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_vaciado_basepallets
boolean visible = false
integer x = 4174
integer y = 1132
integer taborder = 90
long backcolor = 553648127
end type

type pb_grabar from w_mant_directo`pb_grabar within w_vaciado_basepallets
boolean visible = false
integer x = 4174
integer y = 952
integer taborder = 80
long backcolor = 553648127
end type

type dw_1 from w_mant_directo`dw_1 within w_vaciado_basepallets
integer y = 476
integer width = 4005
integer height = 1152
integer taborder = 0
string dataobject = "dw_mues_ordenprocvacdeta_cereza"
boolean vscrollbar = false
boolean livescroll = false
end type

type ole_puerta from olecustomcontrol within w_vaciado_basepallets
event oncomm ( )
integer x = 1970
integer y = 1440
integer width = 174
integer height = 152
boolean bringtotop = true
borderstyle borderstyle = stylelowered!
long backcolor = 8388608
boolean focusrectangle = false
string binarykey = "w_vaciado_basepallets.win"
integer textsize = -40
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 15793151
end type

type dw_estadistica from datawindow within w_vaciado_basepallets
integer x = 270
integer y = 1648
integer width = 3621
integer height = 724
boolean titlebar = true
string title = "Procesos Pendientes"
string dataobject = "dw_vaciados_pendientes_proc"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_encab from uo_dw within w_vaciado_basepallets
boolean visible = false
integer y = 236
integer width = 87
integer height = 64
integer taborder = 0
boolean bringtotop = true
string dataobject = "dw_mant_spro_ordenprocvacenca_cereza"
boolean vscrollbar = false
boolean border = false
end type

type dw_deta from uo_dw within w_vaciado_basepallets
boolean visible = false
integer y = 516
integer width = 87
integer height = 44
integer taborder = 0
boolean bringtotop = true
string dataobject = "dw_mant_mues_spro_ordenprocvacdeta_cereza"
boolean vscrollbar = false
boolean border = false
end type

type sle_tarja from singlelineedit within w_vaciado_basepallets
integer x = 128
integer y = 88
integer width = 1413
integer height = 308
integer taborder = 60
boolean bringtotop = true
integer textsize = -50
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

event modified;Integer	li_largo

IF This.Text	=	'' THEN Return

IF Left(This.Text, 1) = 'H' THEN
	MessageBox("Error", "En este cuadro de texto debe ingresar la tarja del Pallet Granel", Exclamation!)
	This.Text	=	''
	This.SetFocus()
	Return
ELSE
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
	
	IF Message.DoubleParm = -1 THEN
		This.Text = ""
		This.SetFocus()
	END IF
	
END IF
end event

type em_grua from singlelineedit within w_vaciado_basepallets
integer x = 1605
integer y = 88
integer width = 983
integer height = 308
integer taborder = 60
boolean bringtotop = true
integer textsize = -50
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
end type

event modified;Integer	li_largo, li_planta, li_grua
Time		lt_hora

IF This.Text	=	'' THEN Return

IF Left(This.Text, 1) = 'H' THEN
	li_grua	=	Integer(Right(This.Text, Len(This.Text) - 1))
	
	IF NOT iuo_grua.Existe(ii_planta, li_grua, True, Sqlca) THEN
		This.Text	=	''
		This.SetFocus()
		Return
	ELSE
		lt_hora	=	Time(F_fechahora())
		IF NOT iuo_grua.ExisteFecha(ii_planta, li_grua, id_fecha, lt_hora, True, Sqlca) THEN
			This.Text	=	''
			This.SetFocus()
			Return
		END IF
	END IF
ELSE
	MessageBox("Error", "En este cuadro de texto debe ingresar Codigo de Grua", Exclamation!)
	This.Text	=	''
	This.SetFocus()
	Return
END IF
end event

type em_kilos from singlelineedit within w_vaciado_basepallets
integer x = 2665
integer y = 88
integer width = 1367
integer height = 308
integer taborder = 60
boolean bringtotop = true
integer textsize = -50
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 15793151
long backcolor = 8388608
boolean autohscroll = false
integer limit = 9
borderstyle borderstyle = stylelowered!
end type

event modified;id_kilos	=	Dec(This.Text)
end event


Start of PowerBuilder Binary Data Section : Do NOT Edit
03w_vaciado_basepallets.bin 
2B00000600e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe00000006000000000000000000000001000000010000000000001000fffffffe00000000fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
13w_vaciado_basepallets.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
