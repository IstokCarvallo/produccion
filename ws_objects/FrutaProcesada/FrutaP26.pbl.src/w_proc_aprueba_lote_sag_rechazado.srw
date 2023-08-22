$PBExportHeader$w_proc_aprueba_lote_sag_rechazado.srw
$PBExportComments$Genera Archivo Plano SAG de Eliminación de Pallets.
forward
global type w_proc_aprueba_lote_sag_rechazado from w_mant_tabla
end type
type st_3 from statictext within w_proc_aprueba_lote_sag_rechazado
end type
type dw_cliente from datawindow within w_proc_aprueba_lote_sag_rechazado
end type
type st_4 from statictext within w_proc_aprueba_lote_sag_rechazado
end type
type dw_planta from datawindow within w_proc_aprueba_lote_sag_rechazado
end type
type st_numero from statictext within w_proc_aprueba_lote_sag_rechazado
end type
type em_nrosag from editmask within w_proc_aprueba_lote_sag_rechazado
end type
type dw_2 from datawindow within w_proc_aprueba_lote_sag_rechazado
end type
type cbx_1 from checkbox within w_proc_aprueba_lote_sag_rechazado
end type
type dw_destino from datawindow within w_proc_aprueba_lote_sag_rechazado
end type
type st_5 from statictext within w_proc_aprueba_lote_sag_rechazado
end type
type dw_descripcion from datawindow within w_proc_aprueba_lote_sag_rechazado
end type
type st_8 from statictext within w_proc_aprueba_lote_sag_rechazado
end type
type sle_detalle from singlelineedit within w_proc_aprueba_lote_sag_rechazado
end type
end forward

global type w_proc_aprueba_lote_sag_rechazado from w_mant_tabla
integer width = 4311
integer height = 2288
string title = "APROBACIÓN, RECHAZO U OBJECIÓN LOTES SAG"
event ue_validapassword ( )
st_3 st_3
dw_cliente dw_cliente
st_4 st_4
dw_planta dw_planta
st_numero st_numero
em_nrosag em_nrosag
dw_2 dw_2
cbx_1 cbx_1
dw_destino dw_destino
st_5 st_5
dw_descripcion dw_descripcion
st_8 st_8
sle_detalle sle_detalle
end type
global w_proc_aprueba_lote_sag_rechazado w_proc_aprueba_lote_sag_rechazado

type variables
Integer			ii_PlantaSag, il_especie
Date				id_fecha
String			is_descripcion

w_gene_deta_archivo_eliminacion	iw_mantencion

DataWindowChild	idwc_cliente, idwc_planta, dwc_mercado, dwc_descripcion
end variables

forward prototypes
public function integer codigoplantasag (integer planta)
public function boolean noexistecliente (integer ai_cliente)
public function boolean noexisteplanta (integer ai_planta)
protected function boolean wf_actualiza_db ()
public function boolean existeinspeccion (integer ai_cliente, integer ai_planta, long al_numero)
public function boolean existedescripcion (integer ai_especie, integer ai_destino, integer ai_secuencia)
public function boolean existedestino (integer ai_destino)
end prototypes

event ue_validapassword();Str_mant					lstr_mant

lstr_mant.Argumento[1]	=	"Produccion"
lstr_mant.Argumento[2]	=	gs_Password+''+'contraparte'

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

//IF istr_mant.Respuesta = 0 THEN Close(This)
end event

public function integer codigoplantasag (integer planta);Integer	li_Cliente, li_codigo

li_Cliente	=	Integer(istr_mant.Argumento[1])

SELECT	plde_codsag
	INTO	:li_Codigo
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:planta;

RETURN li_Codigo

end function

public function boolean noexistecliente (integer ai_cliente);String	ls_Nombre

SELECT	clie_nombre
	INTO	:ls_Nombre
	FROM	dbo.CLIENTESPROD
	WHERE	:ai_Cliente in (-1,clie_codigo);
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Clientes Producción")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.", Exclamation!, Ok!)
					
	RETURN True
ELSE

	istr_mant.argumento[1]	=	String(ai_cliente)
	RETURN False
END IF
end function

public function boolean noexisteplanta (integer ai_planta);String	ls_Nombre 
Integer  li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[1])

SELECT	plde_nombre, plde_codsag
	INTO	:ls_Nombre, :ii_PlantaSag
	FROM	dbo.PLANTADESP
	WHERE	plde_codigo	=	:ai_Planta;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Plantas y Frigoríficos")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.", Exclamation!, Ok!)
	
	RETURN True
ELSE
	RETURN False
END IF
end function

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
//Integer	li_cliente, li_planta
//Long		ll_inspeccion

//IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
				
//				li_Cliente 		= Integer(istr_mant.argumento[1])
//				li_Planta 		= Integer(istr_mant.argumento[2])
//				ll_inspeccion 	= Long(istr_mant.argumento[3])
//				
//				DECLARE Grabahistoria PROCEDURE FOR dba.Fproc_generainspeccionderechazo
//					@Cliente 	=:li_Cliente,
//					@Planta  	=:li_Planta,
//					@inspeccion =:ll_inspeccion;		
//				EXECUTE Grabahistoria;
				
				Commit;
				
				IF sqlca.sqlcode < 0 THEN
					F_ErrorBaseDatos(sqlca,"El proceso Carga Fproc_generainspeccionderechazo " +&
					" no se ejecutó Exitosamente")
					Return False	
				END IF
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean existeinspeccion (integer ai_cliente, integer ai_planta, long al_numero);Date	ld_fecha
Long	ll_cuenta

SELECT	count(*) 
	INTO	:ll_cuenta
	FROM	dbo.inspecpalenc
	WHERE	:ai_Cliente in (-1,clie_codigo)
	AND	plde_codigo = :ai_planta
	AND	inpe_numero = :al_numero
	AND	inpe_estado = 3;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla inspecpalenc")
	
	RETURN True
ELSEIF ll_cuenta = 0 THEN
	MessageBox("Atención", "Número de inspección no ha sido Rechazado o ya se Encuentra en Estado~r Definitivo .~r~r" + &
					"Ingrese o seleccione otro Número.", Exclamation!, Ok!)
					
	RETURN True
ELSE

	RETURN False
END IF
end function

public function boolean existedescripcion (integer ai_especie, integer ai_destino, integer ai_secuencia);Integer	li_Cliente, li_Planta
Long		ll_Numero

SELECT dsag_descrip
INTO :is_descripcion
FROM dbo.destinossag
WHERE dest_codigo	=	:ai_destino
AND	espe_codigo =  :ai_especie
AND	dsag_secuen =  :ai_secuencia;

IF is_descripcion = '' THEN
	MessageBox("Cuidado","destinossag No Existe")
	RETURN False
END IF
RETURN True	
end function

public function boolean existedestino (integer ai_destino);Integer	li_Cliente, li_Planta
Long		ll_Numero
String	ls

SELECT distinct dest_codigo
INTO :ll_Numero
FROM dbo.destinos
WHERE dest_codigo	=	:ai_destino;

IF IsNull(ll_Numero) OR ll_Numero = 0 THEN
	MessageBox("Cuidado","Destino No Existe")
	RETURN True
END IF
RETURN False	
end function

on w_proc_aprueba_lote_sag_rechazado.create
int iCurrent
call super::create
this.st_3=create st_3
this.dw_cliente=create dw_cliente
this.st_4=create st_4
this.dw_planta=create dw_planta
this.st_numero=create st_numero
this.em_nrosag=create em_nrosag
this.dw_2=create dw_2
this.cbx_1=create cbx_1
this.dw_destino=create dw_destino
this.st_5=create st_5
this.dw_descripcion=create dw_descripcion
this.st_8=create st_8
this.sle_detalle=create sle_detalle
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.dw_planta
this.Control[iCurrent+5]=this.st_numero
this.Control[iCurrent+6]=this.em_nrosag
this.Control[iCurrent+7]=this.dw_2
this.Control[iCurrent+8]=this.cbx_1
this.Control[iCurrent+9]=this.dw_destino
this.Control[iCurrent+10]=this.st_5
this.Control[iCurrent+11]=this.dw_descripcion
this.Control[iCurrent+12]=this.st_8
this.Control[iCurrent+13]=this.sle_detalle
end on

on w_proc_aprueba_lote_sag_rechazado.destroy
call super::destroy
destroy(this.st_3)
destroy(this.dw_cliente)
destroy(this.st_4)
destroy(this.dw_planta)
destroy(this.st_numero)
destroy(this.em_nrosag)
destroy(this.dw_2)
destroy(this.cbx_1)
destroy(this.dw_destino)
destroy(this.st_5)
destroy(this.dw_descripcion)
destroy(this.st_8)
destroy(this.sle_detalle)
end on

event open;call super::open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.InsertRow(0)
//dw_cliente.SetItem(1,"clie_codigo",0)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(-1)
dw_planta.InsertRow(0)
dw_planta.SetItem(1,"plde_codigo",gi_CodPlanta)

dw_destino.GetChild("dest_codigo", dwc_mercado)
dwc_mercado.SetTransObject(Sqlca)
dwc_mercado.Retrieve(-1)
dw_destino.SetTransObject(Sqlca)
dw_destino.InsertRow(0)

dw_1.GetChild("dest_codigo", dwc_mercado)
dwc_mercado.SetTransObject(Sqlca)
dwc_mercado.Retrieve(-1)

dw_descripcion.GetChild("dsag_secuen", dwc_descripcion)
dwc_descripcion.SetTransObject(Sqlca)
dwc_descripcion.Retrieve(gi_CodEspecie,-1)			//especie
dw_descripcion.InsertRow(1)

istr_mant.argumento[1] 	= 	'-1'
istr_mant.argumento[2]	=	String(gi_CodPlanta)
istr_mant.argumento[3]	=	'0'
istr_mant.argumento[4]	=	'0'

dw_2.SetTransObject(sqlca)

end event

event ue_nuevo();//istr_mant.borra			= False
//istr_mant.agrega			= True
//
//OpenWithParm(iw_mantencion, istr_mant)
//
//IF dw_1.RowCount() > 0 AND pb_eliminar.Enabled = FALSE THEN
//	pb_eliminar.Enabled	= TRUE
//	pb_grabar.Enabled		= TRUE
//END IF
//
//dw_1.SetRow(il_fila)
//dw_1.SelectRow(il_fila,True)
//
//
end event

event ue_modifica();//IF dw_1.RowCount() > 0 THEN
//	istr_mant.agrega	= False
//	istr_mant.borra	= False
//
//	OpenWithParm(iw_mantencion, istr_mant)
//END IF
end event

event closequery;//
end event

event ue_recuperadatos;Long	ll_fila, respuesta,ll_fila1

dw_1.Reset()
dw_2.Reset()

DO
	ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]), &
	                         Long(istr_mant.argumento[3]))
	
	ll_fila	= dw_2.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]), &
	                         Long(istr_mant.argumento[3]))
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
//		pb_imprimir.Enabled	= True
//		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
//		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_borrar();//IF dw_1.rowcount() < 1 THEN RETURN
//
//SetPointer(HourGlass!)
//
//ib_borrar = True
//w_main.SetMicroHelp("Validando la eliminación...")
//
//Message.DoubleParm = 0
//
//This.TriggerEvent ("ue_validaborrar")
//
//IF Message.DoubleParm = -1 THEN RETURN
//
//istr_mant.borra		= True
//istr_mant.agrega	= False
//
//OpenWithParm(iw_mantencion, istr_mant)
//
//istr_mant = Message.PowerObjectParm
//
//IF istr_mant.respuesta = 1 THEN
//	IF dw_1.DeleteRow(0) = 1 THEN
//		ib_borrar = False
//		w_main.SetMicroHelp("Borrando Registro...")
//		SetPointer(Arrow!)
//	ELSE
//		ib_borrar = False
//		MessageBox(This.Title,"No se puede borrar actual registro.")
//	END IF
//
// IF dw_1.RowCount() = 0 THEN
//		pb_eliminar.Enabled = False
//	ELSE
//		il_fila = dw_1.GetRow()
//		dw_1.SelectRow(il_fila,True)
//	END IF
//END IF
//
//istr_mant.borra	 = False
end event

event ue_antesguardar;call super::ue_antesguardar;Integer		li_Cliente, li_Planta, li_tipoin
Long			ll_Fila, ll_Numero, ll_numinp, ll_null
Date			ld_fechai
String		ls_fecha


SetNull(ll_null)

FOR ll_Fila = 1 TO dw_1.RowCount()
	
	li_Cliente	=	dw_1.Object.Clie_codigo[ll_Fila]
	li_Planta	=	dw_1.Object.Plde_codigo[ll_Fila]
	ll_Numero	=	dw_1.Object.Paen_numero[ll_Fila]
	li_tipoin	=	dw_1.Object.inpe_tipoin[ll_Fila]
	ll_Numinp	=	dw_1.Object.inpe_numero[ll_Fila]
	ld_fechai 	= dw_2.Object.inpd_fechai[1]
	
	
	//aprobado
	dw_1.Object.paen_inspec[ll_Fila]	=	1
	dw_1.Object.inpe_numero[ll_Fila]	=	Long(em_nrosag.Text)
	dw_1.Object.inpe_tipoin[ll_Fila]	=	1
	dw_1.Object.inpe_fechai[ll_Fila]	=  ld_fechai
	dw_1.Object.Dest_codigo[ll_fila] =  dw_destino.Object.dest_codigo[1]
	dw_2.Object.inpd_frecha[ll_Fila]	=	Date(19000101)
	dw_2.Object.dest_codigo[ll_Fila]	=	dw_destino.Object.dest_codigo[1]
	
	dw_1.Object.paen_fecter[ll_Fila] = Datetime(Today(), Now())
	
NEXT


end event

event ue_guardar;Integer	li_cliente, li_planta, li_estado, li_destino, li_secuencia
Long		ll_numero
Date		ld_fecres
String	ls_detalle

li_cliente 	= 	Integer(istr_mant.argumento[1])
li_planta	=	Integer(istr_mant.argumento[2])
ll_numero	=	Long(istr_mant.argumento[3])
li_destino = 	dw_destino.Object.dest_codigo[1]
li_estado = 1
li_secuencia = dw_descripcion.Object.dsag_secuen[1]
ls_detalle = sle_detalle.Text

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	
	UPDATE dbo.inspecpalenc SET
		inpe_estado = 1,	
		dest_codigo = :li_destino,
		inpe_dessec = :li_secuencia,
		inpe_desdet = :ls_detalle
		WHERE :li_cliente in (-1,clie_codigo)
		AND	plde_codigo = :li_planta
		AND	inpe_numero = :ll_numero;
		COMMIT;
	
	dw_1.Reset()
	dw_2.Reset()
	MessageBox("Atención", "Datos Grabados Exitosamente", Exclamation!, Ok!)
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_proc_aprueba_lote_sag_rechazado
integer y = 676
integer width = 3191
integer height = 1092
integer taborder = 0
string dataobject = "dw_mues_apruebarechazo_lote_sag"
boolean hscrollbar = true
end type

event dw_1::clicked;call super::clicked;String		ls_Tecla

IF KeyDown(KeyShift!) THEN
	ls_tecla	=	"Shift"
ELSEIF KeyDown(KeyControl!) THEN
	ls_tecla	=	"Control"
END IF

F_Selecciona(This, ls_Tecla, Row)

IF dw_1.GetSelectedRow(0) = 0 THEN
	pb_grabar.Enabled	=	False
ELSE
	pb_grabar.Enabled	=	True
END IF

end event

event dw_1::getfocus;//
end event

event dw_1::losefocus;//
end event

event dw_1::rowfocuschanged;//
end event

event type long dw_1::ue_seteafila(unsignedlong wparam, long lparam);RETURN 0  
end event

type st_encabe from w_mant_tabla`st_encabe within w_proc_aprueba_lote_sag_rechazado
integer y = 20
integer width = 3214
integer height = 644
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_proc_aprueba_lote_sag_rechazado
integer x = 3401
integer y = 188
integer taborder = 0
boolean enabled = false
end type

event pb_lectura::clicked;IF cbx_1.Checked THEN
	istr_mant.argumento[1] 	= 	'-1'
ELSE
	istr_mant.argumento[1] 	= 	String(dw_cliente.Object.clie_codigo[1])
END IF	

istr_mant.argumento[2]	=	String(dw_planta.Object.plde_codigo[1])
istr_mant.argumento[3]	=	em_nrosag.Text

IF NOT existeinspeccion(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]), &
	                         Long(istr_mant.argumento[3])) THEN
	Parent.TriggerEvent("ue_recuperadatos")
ELSE									 
	em_nrosag.Text = ''
	em_nrosag.SetFocus()
END IF
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_proc_aprueba_lote_sag_rechazado
integer x = 3397
integer y = 488
integer taborder = 50
end type

event pb_nuevo::clicked;String ls_Null

SetNull (ls_Null)

pb_insertar.Enabled	= 	False
pb_imprimir.Enabled	= 	False
pb_eliminar.Enabled	= 	False
pb_grabar.Enabled		= 	False
pb_lectura.Enabled	=	False

em_nrosag.Text = ls_Null
em_nrosag.SetFocus()
dw_1.Reset()
dw_2.Reset()

//istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_proc_aprueba_lote_sag_rechazado
boolean visible = false
integer x = 3397
integer y = 660
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_proc_aprueba_lote_sag_rechazado
boolean visible = false
integer x = 3397
integer y = 840
integer taborder = 60
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_proc_aprueba_lote_sag_rechazado
integer x = 3401
integer y = 1024
integer taborder = 70
end type

event pb_grabar::clicked;Integer	li_null

SetNull(li_null)

IF isnull(dw_destino.Object.dest_codigo[1]) THEN
	MessageBox("Atención", "Código de Destino no ha sido Ingresado.~r~r" + &
					"Seleccione un Destino.", Exclamation!, Ok!)
	dw_destino.SetItem(1, "dest_codigo", li_null)				
	Return 1
END IF	

IF isnull(dw_descripcion.Object.dsag_secuen[1]) THEN
	MessageBox("Atención", "Código de Secuencia Destino no ha sido Ingresado.~r~r" + &
					"Seleccione un Secuencia.", Exclamation!, Ok!)
	dw_descripcion.SetItem(1, "dsag_secuen", li_null)				
	Return 1
END IF	

IF sle_detalle.Text = '' THEN
	MessageBox("Atención", "Detalle Destino no ha sido Ingresado.~r~r" , Exclamation!, Ok!)
	sle_detalle.Text = ''			
	Return 1
END IF	

Parent.TriggerEvent("ue_guardar")
end event

type pb_imprimir from w_mant_tabla`pb_imprimir within w_proc_aprueba_lote_sag_rechazado
boolean visible = false
integer x = 3397
integer y = 1196
integer taborder = 80
end type

type pb_salir from w_mant_tabla`pb_salir within w_proc_aprueba_lote_sag_rechazado
integer x = 3397
integer y = 1584
integer taborder = 90
end type

type st_3 from statictext within w_proc_aprueba_lote_sag_rechazado
integer x = 297
integer y = 44
integer width = 448
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

type dw_cliente from datawindow within w_proc_aprueba_lote_sag_rechazado
integer x = 1074
integer y = 32
integer width = 1152
integer height = 92
integer taborder = 10
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event dberror;RETURN 1
end event

event itemchanged;IF NoExisteCliente(Integer(data)) THEN
	This.SetItem(1, "clie_codigo", gi_codexport)
	
	RETURN 1
ELSE
	istr_mant.argumento[1]	=	String(Integer(data), '000')
	idwc_planta.Retrieve(1)
	istr_mant.argumento[2]	=	String(dw_planta.Object.plde_codigo[1])
	dw_planta.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
END IF
end event

type st_4 from statictext within w_proc_aprueba_lote_sag_rechazado
integer x = 288
integer y = 132
integer width = 453
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
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_proc_aprueba_lote_sag_rechazado
integer x = 1079
integer y = 124
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event dberror;RETURN 1
end event

event itemchanged;IF NoExistePlanta(Integer(data)) THEN
	This.SetItem(1, "plde_codigo", gi_codplanta)
	
	RETURN 1
ELSE
	istr_mant.argumento[2]	=	String(data)
END IF
end event

type st_numero from statictext within w_proc_aprueba_lote_sag_rechazado
integer x = 288
integer y = 232
integer width = 695
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
string text = "Nro. Inspección S.A.G."
boolean focusrectangle = false
end type

type em_nrosag from editmask within w_proc_aprueba_lote_sag_rechazado
integer x = 1083
integer y = 212
integer width = 462
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Date	ld_fecha
Long	ll_numero
Integer	li_planta, li_cliente

ll_numero = Long(This.Text)

li_planta = dw_planta.Object.plde_codigo[1]

IF cbx_1.Checked THEN
	li_cliente = -1
ELSE
	li_cliente = dw_cliente.Object.clie_codigo[1]
END IF	

SELECT	distinct espe_codigo 
	INTO	:il_especie
	FROM	dbo.inspecpalenc
	WHERE	:li_cliente in (-1,clie_codigo)
	AND	plde_codigo = :li_planta
	AND	inpe_numero = :ll_numero
	AND	inpe_estado = 3;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla inspecpalenc")
END IF

istr_mant.argumento[3]	=	This.Text

IF Long(This.Text) > 0 THEN
	pb_lectura.Enabled	=	True
END IF
end event

type dw_2 from datawindow within w_proc_aprueba_lote_sag_rechazado
boolean visible = false
integer x = 69
integer y = 1956
integer width = 2546
integer height = 748
integer taborder = 100
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_apruebarechazo_lote_sag_2"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type cbx_1 from checkbox within w_proc_aprueba_lote_sag_rechazado
integer x = 2295
integer y = 40
integer width = 311
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[1] = '-1'
	dw_cliente.Enabled = False	
	dw_cliente.Reset()
	idwc_cliente.Retrieve()
	dw_cliente.InsertRow(0)
ELSE
	dw_cliente.Enabled = True
	idwc_cliente.Retrieve()
	dw_cliente.InsertRow(0)
	dw_cliente.SetItem(1,"clie_codigo",	gi_CodExport)
	istr_mant.argumento[1] = String(gi_CodExport)
END IF	
end event

type dw_destino from datawindow within w_proc_aprueba_lote_sag_rechazado
integer x = 1074
integer y = 308
integer width = 882
integer height = 92
integer taborder = 80
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_destinos"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_columna
Integer	li_null

SetNull(li_null)

ls_columna	= dwo.Name

CHOOSE CASE ls_columna
	CASE "dest_codigo"
		IF existedestino(Integer(data)) THEN
			dw_descripcion.SetItem(1, "dsag_secuen", li_null)
			This.SetItem(1, "dest_codigo", li_null)
			dw_descripcion.Enabled = False
			RETURN 1
		ELSE
			dw_descripcion.GetChild("dsag_secuen", dwc_descripcion)
			dwc_descripcion.SetTransObject(Sqlca)
			
			IF isnull(il_Especie) THEN
				IF dwc_descripcion.Retrieve(-1,integer(data)) = 0 THEN
					dw_descripcion.SetItem(1, "dsag_secuen", li_null)
					MessageBox("Atención", "No Existe Secuencia para ese Destino, Haga Mantención Tabla Destinos Sag.", &
					Exclamation!, OK!)
					This.SetItem(1, "dest_codigo", li_null)
					Return 
				END IF	
			ELSE
				IF dwc_descripcion.Retrieve(il_Especie,integer(data)) = 0 THEN
					dw_descripcion.SetItem(1, "dsag_secuen", li_null)
					MessageBox("Atención", "No Existe Secuencia para ese Destino, Haga Mantención Tabla Destinos Sag.", &
					Exclamation!, OK!)
					This.SetItem(1, "dest_codigo", li_null)
					Return 
				END IF	
					dw_descripcion.Enabled = True
			END IF	
			//dw_descripcion.InsertRow(1)
								
		END IF	
		
END CHOOSE

end event

type st_5 from statictext within w_proc_aprueba_lote_sag_rechazado
integer x = 288
integer y = 320
integer width = 430
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
string text = "Nuevo Destino"
boolean focusrectangle = false
end type

type dw_descripcion from datawindow within w_proc_aprueba_lote_sag_rechazado
integer x = 1070
integer y = 396
integer width = 855
integer height = 92
integer taborder = 80
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_destinosag"
boolean border = false
boolean livescroll = true
end type

event itemchanged;IF existedescripcion(il_especie,dw_destino.Object.dest_codigo[1],Integer(data)) THEN
	sle_detalle.Text = is_descripcion
END IF
end event

type st_8 from statictext within w_proc_aprueba_lote_sag_rechazado
integer x = 293
integer y = 512
integer width = 453
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Detalle Destino"
boolean focusrectangle = false
end type

type sle_detalle from singlelineedit within w_proc_aprueba_lote_sag_rechazado
integer x = 773
integer y = 504
integer width = 2025
integer height = 92
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
integer limit = 600
borderstyle borderstyle = stylelowered!
end type

event modified;istr_mant.argumento[7]	=	This.Text
end event

