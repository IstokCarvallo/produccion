$PBExportHeader$w_carga_despacho_inspecciones.srw
forward
global type w_carga_despacho_inspecciones from window
end type
type st_2 from statictext within w_carga_despacho_inspecciones
end type
type dw_4 from datawindow within w_carga_despacho_inspecciones
end type
type dw_3 from datawindow within w_carga_despacho_inspecciones
end type
type pb_abrir from picturebutton within w_carga_despacho_inspecciones
end type
type dw_11 from datawindow within w_carga_despacho_inspecciones
end type
type dw_10 from datawindow within w_carga_despacho_inspecciones
end type
type st_4 from statictext within w_carga_despacho_inspecciones
end type
type st_3 from statictext within w_carga_despacho_inspecciones
end type
type sle_mensa from singlelineedit within w_carga_despacho_inspecciones
end type
type st_5 from statictext within w_carga_despacho_inspecciones
end type
type st_1 from statictext within w_carga_despacho_inspecciones
end type
type pb_salir from picturebutton within w_carga_despacho_inspecciones
end type
type pb_grabar from picturebutton within w_carga_despacho_inspecciones
end type
type st_6 from statictext within w_carga_despacho_inspecciones
end type
type dw_1 from datawindow within w_carga_despacho_inspecciones
end type
type dw_2 from datawindow within w_carga_despacho_inspecciones
end type
end forward

global type w_carga_despacho_inspecciones from window
integer width = 2889
integer height = 1188
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
string icon = "TABLA.ICO"
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
st_2 st_2
dw_4 dw_4
dw_3 dw_3
pb_abrir pb_abrir
dw_11 dw_11
dw_10 dw_10
st_4 st_4
st_3 st_3
sle_mensa sle_mensa
st_5 st_5
st_1 st_1
pb_salir pb_salir
pb_grabar pb_grabar
st_6 st_6
dw_1 dw_1
dw_2 dw_2
end type
global w_carga_despacho_inspecciones w_carga_despacho_inspecciones

type variables
str_mant               istr_mant
Date		id_FechaAcceso
Time		it_HoraAcceso

DataWindowChild	idwc_cliente, idwc_planta
end variables

forward prototypes
public function boolean existeplanilla (long al_planilla)
public function string clienterotulado (integer cliente)
public function boolean wf_actualiza_db (boolean borrando)
end prototypes

event ue_guardar();Long	ll_fila, ll_numero
Integer	li_planta, li_cliente


IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	
	FOR ll_fila = 1 TO dw_2.RowCount()
		li_planta 	= dw_2.Object.plde_codigo[ll_fila]
		li_cliente	= dw_2.Object.clie_codigo[ll_fila]
		ll_numero	= dw_2.Object.paen_numero[ll_fila]
		
		UPDATE dbo.palletencab SET
		paen_inspec = 1
		WHERE clie_codigo = :li_cliente
		AND plde_codigo = :li_planta
		AND paen_numero = :ll_numero;
		
	NEXT
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

event ue_antesguardar;Long 		ll_fila, ll_numero, ll_Existe, ll_pallet, ll_nuevo, ll_det, ll_Existepallet 
Integer	li_cliente, li_Planta, li_secuen, li_tipoin
String	ls_numero

FOR ll_fila = 1 TO dw_3.RowCount()
	IF dw_3.Object.inpe_numero[ll_fila] > 9999 THEN
		ls_numero = Right(String(dw_3.Object.inpe_numero[ll_fila]), 4)
		
		ll_numero		=	dw_3.Object.plde_codigo[ll_fila] * 1000000 + Long(ls_numero)
	ELSE	
		ll_numero		=	dw_3.Object.plde_codigo[ll_fila] * 1000000 + dw_3.Object.inpe_numero[ll_fila]
	END IF
	
	li_cliente		= 	dw_3.Object.clie_codigo[ll_fila]
	li_tipoin		= 	dw_3.Object.inpe_tipoin[ll_fila]
	li_planta		= 	Integer(istr_mant.argumento[2])
	li_secuen		=	dw_3.Object.inpe_secuen[ll_fila]
	
	SELECT Count(*)
	INTO :ll_Existe
	FROM dbo.inspecpalenc
	WHERE inpe_tipoin = :li_tipoin  AND
			inpe_numero = :ll_numero  AND
			clie_codigo = :li_Cliente AND
			plde_codigo = :li_Planta  AND
			inpe_secuen = :li_secuen;
			
	IF ll_existe = 0 THEN
		dw_3.Object.inpe_numero[ll_fila] = ll_numero
		dw_3.Object.plde_codigo[ll_fila] = Integer(istr_mant.argumento[2])
		
		ll_nuevo = dw_1.InsertRow(0)
		dw_1.Object.inpe_tipoin[ll_nuevo] = dw_3.Object.inpe_tipoin[ll_fila]
		dw_1.Object.inpe_numero[ll_nuevo] = dw_3.Object.inpe_numero[ll_fila]
		dw_1.Object.clie_codigo[ll_nuevo] = dw_3.Object.clie_codigo[ll_fila]
		dw_1.Object.plde_codigo[ll_nuevo] = dw_3.Object.plde_codigo[ll_fila]
		dw_1.Object.inpe_secuen[ll_nuevo] = dw_3.Object.inpe_secuen[ll_fila]
		dw_1.Object.dest_codigo[ll_nuevo] = dw_3.Object.dest_codigo[ll_fila]
		dw_1.Object.inpe_fechai[ll_nuevo] = dw_3.Object.inpe_fechai[ll_fila]
		dw_1.Object.espe_codigo[ll_nuevo] = dw_3.Object.espe_codigo[ll_fila]
		dw_1.Object.vari_codigo[ll_nuevo] = dw_3.Object.vari_codigo[ll_fila]
		dw_1.Object.emba_codigo[ll_nuevo] = dw_3.Object.emba_codigo[ll_fila]
		dw_1.Object.tpem_codigo[ll_nuevo] = dw_3.Object.tpem_codigo[ll_fila]
		dw_1.Object.inpe_todpal[ll_nuevo] = dw_3.Object.inpe_todpal[ll_fila]
		dw_1.Object.inpe_calibr[ll_nuevo] = dw_3.Object.inpe_calibr[ll_fila]
		dw_1.Object.inpe_solnom[ll_nuevo] = dw_3.Object.inpe_solnom[ll_fila]
		dw_1.Object.inpe_fecini[ll_nuevo] = dw_3.Object.inpe_fecini[ll_fila]
		dw_1.Object.inpe_fecter[ll_nuevo] = dw_3.Object.inpe_fecter[ll_fila]
		dw_1.Object.inpe_proces[ll_nuevo] = dw_3.Object.inpe_proces[ll_fila]
		dw_1.Object.inpe_estado[ll_nuevo] = dw_3.Object.inpe_estado[ll_fila]
		dw_1.Object.tran_fechat[ll_nuevo] = dw_3.Object.tran_fechat[ll_fila]
		dw_1.Object.inpe_desdet[ll_nuevo] = dw_3.Object.inpe_desdet[ll_fila]
		dw_1.Object.inpe_fecres[ll_nuevo] = dw_3.Object.inpe_fecres[ll_fila]
		dw_1.Object.inpe_dessec[ll_nuevo] = dw_3.Object.inpe_dessec[ll_fila]
	END IF
NEXT	

ll_fila = 0

FOR ll_fila = 1 TO dw_4.RowCount()
	
	
	IF dw_4.Object.inpe_numero[ll_fila] > 9999 THEN
		ls_numero = Right(String(dw_4.Object.inpe_numero[ll_fila]), 4)
		
		ll_numero		=	dw_4.Object.plde_codigo[ll_fila] * 1000000 + Long(ls_numero)
	ELSE	
		ll_numero		=	dw_4.Object.plde_codigo[ll_fila] * 1000000 + dw_4.Object.inpe_numero[ll_fila]
	END IF
	
	li_cliente		= 	dw_4.Object.clie_codigo[ll_fila]
	li_tipoin		= 	dw_4.Object.inpe_tipoin[ll_fila]
	li_planta		= 	Integer(istr_mant.argumento[2])
	li_secuen		=	dw_4.Object.inpe_secuen[ll_fila]
	ll_pallet		= 	dw_4.Object.paen_numero[ll_fila]
	
	SELECT Count(*)
	INTO :ll_Existepallet
	FROM dbo.palletencab
	WHERE paen_numero = :ll_pallet  AND
			plde_codigo = :li_Planta  AND
			clie_codigo = :li_Cliente;
			
	IF ll_Existepallet = 0 THEN	
		MessageBox("Atención", "No Existe Número Pallet en Planta.", &
						Exclamation!, Ok!)
		Message.DoubleParm = -1
		Return 1
	END IF	
	
	SELECT Count(*)
	INTO :ll_Existe
	FROM dbo.inspecpaldet
	WHERE inpe_tipoin = :li_tipoin  AND
			inpe_numero = :ll_numero  AND
			clie_codigo = :li_Cliente AND
			plde_codigo = :li_Planta  AND
			inpe_secuen = :li_secuen  AND
			paen_numero = :ll_pallet;
	
	IF ll_existe = 0 THEN
		dw_4.Object.plde_codigo[ll_fila] = Integer(istr_mant.argumento[2])
		dw_4.Object.inpe_numero[ll_fila] = ll_numero
		
		ll_det = dw_2.InsertRow(0)
		
		dw_2.Object.inpe_tipoin[ll_det] = dw_4.Object.inpe_tipoin[ll_fila]
		dw_2.Object.inpe_numero[ll_det] = dw_4.Object.inpe_numero[ll_fila]
		dw_2.Object.clie_codigo[ll_det] = dw_4.Object.clie_codigo[ll_fila]
		dw_2.Object.plde_codigo[ll_det] = dw_4.Object.plde_codigo[ll_fila]
		dw_2.Object.inpe_secuen[ll_det] = dw_4.Object.inpe_secuen[ll_fila]
		dw_2.Object.paen_numero[ll_det] = dw_4.Object.paen_numero[ll_fila]
		dw_2.Object.dest_codigo[ll_det] = dw_4.Object.dest_codigo[ll_fila]
		dw_2.Object.inpd_fechai[ll_det] = dw_4.Object.inpd_fechai[ll_fila]
		dw_2.Object.inpd_nroanu[ll_det] = dw_4.Object.inpd_nroanu[ll_fila]
		dw_2.Object.inpd_fechaa[ll_det] = dw_4.Object.inpd_fechaa[ll_fila]
		dw_2.Object.inpd_frecha[ll_det] = dw_4.Object.inpd_frecha[ll_fila]
		dw_2.Object.tran_fechat[ll_det] = dw_4.Object.tran_fechat[ll_fila]
		dw_2.Object.inpd_nrodev[ll_det] = dw_4.Object.inpd_nrodev[ll_fila]
		dw_2.Object.inpd_fecdev[ll_det] = dw_4.Object.inpd_fecdev[ll_fila]
		dw_2.Object.inpd_nrocer[ll_det] = dw_4.Object.inpd_nrocer[ll_fila]
		dw_2.Object.inpd_fecres[ll_det] = dw_4.Object.inpd_fecres[ll_fila]	
	END IF	
NEXT	
end event

public function boolean existeplanilla (long al_planilla);//Integer	li_codexp, li_planta
//Date		ld_fecha
//
//li_codexp		=	Integer(istr_mant.argumento[1])
//li_planta		=	Integer(istr_mant.argumento[2])
//
//IF (al_planilla <> 0) OR li_planta = 0 THEN
//	
//	SELECT Max(defe_fecdes)
//		INTO	:ld_fecha
//		FROM	dbo.DESPAFRIGOEN
//		WHERE	plde_codigo =	:li_planta
//		AND	clie_codigo	=	:li_codexp
//		AND	defe_numero	=	:al_planilla;
//				
//	IF sqlca.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla DESPAFRIGOEN")
//		em_planilla.SetFocus()
//		RETURN False
//	ELSEIF sqlca.SQLCode = 100 THEN
//		MessageBox("Atención", "No Existe Despacho. Indicado.~r~rIngrese otro Número.", &
//						Exclamation!, Ok!)
//		pb_grabar.Enabled	= False
//		em_planilla.SetFocus()
//		RETURN False
//	ELSEIF IsNull(ld_fecha) THEN
//		MessageBox("Atención", "No existe Despacho. Indicado.~r~rIngrese otro Número.", &
//						Exclamation!, Ok!)
//		pb_grabar.Enabled	= False
//		em_planilla.SetFocus()
//		RETURN False
//	ELSE
//		
//		sle_mensa.text		= ""
//		pb_grabar.Enabled	= True
//		RETURN True
//	END IF
//ELSE
//	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
//					Exclamation!, Ok!)
	RETURN False
//END IF
end function

public function string clienterotulado (integer cliente);String	ls_rotulacion

SELECT clie_rotula
INTO   :ls_rotulacion
FROM dbo.clientesprod
WHERE clie_codigo = :Cliente;

IF IsNull(ls_rotulacion) THEN
	
	ls_rotulacion	=	String(Cliente)

END IF

RETURN ls_rotulacion
end function

public function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				sle_mensa.Text = 'Error en Grabación de Datos'			
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
				
				sle_mensa.Text = 'Proceso Terminado'
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

on w_carga_despacho_inspecciones.create
this.st_2=create st_2
this.dw_4=create dw_4
this.dw_3=create dw_3
this.pb_abrir=create pb_abrir
this.dw_11=create dw_11
this.dw_10=create dw_10
this.st_4=create st_4
this.st_3=create st_3
this.sle_mensa=create sle_mensa
this.st_5=create st_5
this.st_1=create st_1
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.st_6=create st_6
this.dw_1=create dw_1
this.dw_2=create dw_2
this.Control[]={this.st_2,&
this.dw_4,&
this.dw_3,&
this.pb_abrir,&
this.dw_11,&
this.dw_10,&
this.st_4,&
this.st_3,&
this.sle_mensa,&
this.st_5,&
this.st_1,&
this.pb_salir,&
this.pb_grabar,&
this.st_6,&
this.dw_1,&
this.dw_2}
end on

on w_carga_despacho_inspecciones.destroy
destroy(this.st_2)
destroy(this.dw_4)
destroy(this.dw_3)
destroy(this.pb_abrir)
destroy(this.dw_11)
destroy(this.dw_10)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.sle_mensa)
destroy(this.st_5)
destroy(this.st_1)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.st_6)
destroy(this.dw_1)
destroy(this.dw_2)
end on

event open;x	=	0
y	=	0
This.Icon	=	Gstr_apl.Icono

dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

dw_10.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_10.InsertRow(0)
dw_10.SetItem(1,"clie_codigo",gi_codexport)

dw_11.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_11.InsertRow(0)
dw_11.SetItem(1,"plde_codigo",gi_codplanta)

istr_mant.argumento[1]	=	String(gi_codexport,'000')
istr_mant.argumento[2]	=	String(gi_codplanta)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							


end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type st_2 from statictext within w_carga_despacho_inspecciones
integer x = 82
integer y = 252
integer width = 2318
integer height = 80
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 65535
long backcolor = 16711680
boolean enabled = false
string text = "Importante!! Los Archivos deben estar contenidos en carpeta Mis Documentos"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type dw_4 from datawindow within w_carga_despacho_inspecciones
boolean visible = false
integer x = 457
integer y = 1324
integer width = 274
integer height = 200
integer taborder = 40
string title = "none"
string dataobject = "dw_despacho_inspecciones_det"
end type

type dw_3 from datawindow within w_carga_despacho_inspecciones
boolean visible = false
integer x = 142
integer y = 1096
integer width = 274
integer height = 200
integer taborder = 50
string title = "none"
string dataobject = "dw_despacho_inspecciones_enc"
end type

type pb_abrir from picturebutton within w_carga_despacho_inspecciones
integer x = 2473
integer y = 332
integer width = 302
integer height = 244
integer taborder = 40
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Abrir Carpeta.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Abrir Carpeta-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;String	ls_directorio, ls_archivo, path, nombre, pathpordefault, ls_ruta, ls, ls_fecha
Integer	li_valida, li_opcion = 1,li_Numero, li_Retorno
Long		ll_rc, li_ret
Boolean	lb_retorno
Date		ld_fdesde, ld_fhasta

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)

IF GetFileOpenName ("Carga Archivo", path, nombre,  "csv", 'inspecpalenc*.CSV,inspecpalenc*.CSV', ls_ruta,18)< 1 THEN Return

sle_mensa.Text = 'Proceso : Genera Tabla Inspecpalenc. (Inspecpalenc)'

ls_archivo = Right(nombre, 12)

li_ret = dw_3.ImportFile(ls_ruta + "\" + 'Inspecpalenc'+ls_archivo)

IF li_ret < 1 THEN
	MessageBox("Error", "No se pudo Cargar Detalle de Inspección")
	lb_retorno = False
END IF

li_ret = dw_4.ImportFile(ls_ruta + "\" + 'Inspecpaldet'+ls_archivo)
IF li_ret < 1 THEN
	MessageBox("Error", "No se pudo Cargar Encabezado del Movimiento")
	lb_retorno = False
END IF
sle_mensa.Text = 'Inspección : Genera Tablas de Inspecciones.'

IF dw_3.RowCount() > 0 THEN
	pb_grabar.Enabled = True
END IF


end event

type dw_11 from datawindow within w_carga_despacho_inspecciones
integer x = 576
integer y = 660
integer width = 969
integer height = 100
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[2]	=	String(data)
end event

type dw_10 from datawindow within w_carga_despacho_inspecciones
integer x = 576
integer y = 488
integer width = 1157
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	String(Integer(data),'000')
idwc_planta.Retrieve(1)
istr_mant.argumento[2]	=	String(dw_11.Object.plde_codigo[1])
dw_11.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
end event

type st_4 from statictext within w_carga_despacho_inspecciones
integer x = 133
integer y = 668
integer width = 402
integer height = 64
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_carga_despacho_inspecciones
integer x = 133
integer y = 496
integer width = 402
integer height = 64
integer taborder = 20
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

type sle_mensa from singlelineedit within w_carga_despacho_inspecciones
integer x = 178
integer y = 908
integer width = 2149
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
boolean italic = true
long textcolor = 65535
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_carga_despacho_inspecciones
integer x = 82
integer y = 100
integer width = 2318
integer height = 140
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
string text = "Carga Archivo Plano Despacho (Inspecciones)"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_1 from statictext within w_carga_despacho_inspecciones
integer x = 82
integer y = 424
integer width = 2318
integer height = 416
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_carga_despacho_inspecciones
integer x = 2473
integer y = 840
integer width = 302
integer height = 244
integer taborder = 70
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_carga_despacho_inspecciones
integer x = 2473
integer y = 584
integer width = 302
integer height = 244
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Parent.TriggerEvent("ue_guardar")
end event

type st_6 from statictext within w_carga_despacho_inspecciones
integer x = 82
integer y = 840
integer width = 2318
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_1 from datawindow within w_carga_despacho_inspecciones
boolean visible = false
integer x = 448
integer y = 1092
integer width = 274
integer height = 200
boolean bringtotop = true
string dataobject = "dw_despacho_inspecciones_enc"
end type

type dw_2 from datawindow within w_carga_despacho_inspecciones
boolean visible = false
integer x = 146
integer y = 1320
integer width = 274
integer height = 200
integer taborder = 90
boolean bringtotop = true
string dataobject = "dw_despacho_inspecciones_det"
end type

event clicked;//This.Print()
end event

