$PBExportHeader$w_mant_mues_inspecpalenc_nuevo.srw
forward
global type w_mant_mues_inspecpalenc_nuevo from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_inspecpalenc_nuevo
end type
type st_2 from statictext within w_mant_mues_inspecpalenc_nuevo
end type
type ddlb_tipocond from dropdownlistbox within w_mant_mues_inspecpalenc_nuevo
end type
type st_4 from statictext within w_mant_mues_inspecpalenc_nuevo
end type
type st_3 from statictext within w_mant_mues_inspecpalenc_nuevo
end type
type em_numero from editmask within w_mant_mues_inspecpalenc_nuevo
end type
type cb_detalle from commandbutton within w_mant_mues_inspecpalenc_nuevo
end type
type st_5 from statictext within w_mant_mues_inspecpalenc_nuevo
end type
type st_6 from statictext within w_mant_mues_inspecpalenc_nuevo
end type
type em_fecha from editmask within w_mant_mues_inspecpalenc_nuevo
end type
type st_7 from statictext within w_mant_mues_inspecpalenc_nuevo
end type
type sle_solic from singlelineedit within w_mant_mues_inspecpalenc_nuevo
end type
type st_22 from statictext within w_mant_mues_inspecpalenc_nuevo
end type
type em_desde from editmask within w_mant_mues_inspecpalenc_nuevo
end type
type st_23 from statictext within w_mant_mues_inspecpalenc_nuevo
end type
type em_hasta from editmask within w_mant_mues_inspecpalenc_nuevo
end type
type cbx_fumigacion from checkbox within w_mant_mues_inspecpalenc_nuevo
end type
type st_9 from statictext within w_mant_mues_inspecpalenc_nuevo
end type
type sle_observacion from singlelineedit within w_mant_mues_inspecpalenc_nuevo
end type
type cbx_facturable from checkbox within w_mant_mues_inspecpalenc_nuevo
end type
type st_10 from statictext within w_mant_mues_inspecpalenc_nuevo
end type
type uo_selplantas from uo_seleccion_plantas within w_mant_mues_inspecpalenc_nuevo
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_inspecpalenc_nuevo
end type
type uo_seldestino from uo_seleccion_destinos within w_mant_mues_inspecpalenc_nuevo
end type
end forward

global type w_mant_mues_inspecpalenc_nuevo from w_mant_tabla
integer width = 3776
integer height = 1964
string title = "Solicitudes de Inspección - Re-Inspección"
st_1 st_1
st_2 st_2
ddlb_tipocond ddlb_tipocond
st_4 st_4
st_3 st_3
em_numero em_numero
cb_detalle cb_detalle
st_5 st_5
st_6 st_6
em_fecha em_fecha
st_7 st_7
sle_solic sle_solic
st_22 st_22
em_desde em_desde
st_23 st_23
em_hasta em_hasta
cbx_fumigacion cbx_fumigacion
st_9 st_9
sle_observacion sle_observacion
cbx_facturable cbx_facturable
st_10 st_10
uo_selplantas uo_selplantas
uo_selcliente uo_selcliente
uo_seldestino uo_seldestino
end type
global w_mant_mues_inspecpalenc_nuevo w_mant_mues_inspecpalenc_nuevo

type variables
w_mant_deta_inspecpalenc	iw_mantencion

DataWindowChild				dwc_especie, dwc_variedad, dwc_embalaje, dwc_tipopalemb
end variables

forward prototypes
public subroutine wf_opciontodas ()
public function boolean existeinspeccion (long al_numero)
protected function boolean wf_actualiza_db ()
end prototypes

public subroutine wf_opciontodas ();Long		ll_fila
Integer	li_null
String	ls_null

SetNull(li_null)
SetNull(ls_null)

//Inserción de Opción Todas

ll_fila	= dwc_especie.InsertRow(1)
dwc_especie.SetItem(ll_fila,"espe_codigo",li_null)
dwc_especie.SetItem(ll_fila,"espe_nombre","TODAS")

ll_fila	= dwc_variedad.InsertRow(1)
dwc_variedad.SetItem(ll_fila,"vari_codigo",li_null)
dwc_variedad.SetItem(ll_fila,"vari_nombre","TODAS")
end subroutine

public function boolean existeinspeccion (long al_numero);Integer	li_Cliente, li_Planta, li_Tipo
Long		ll_Numero

//li_Cliente	=	Integer(istr_mant.argumento[1])

li_Cliente	=	uo_SelCliente.Codigo
li_Planta	=	uo_SelPlantas.Codigo
li_Tipo		=	Integer(istr_mant.argumento[3])

SELECT distinct inpe_numero
INTO :ll_Numero
FROM dbo.Inspecpaldet
WHERE clie_codigo	=	:li_Cliente
AND   plde_codigo =	:li_Planta
AND   inpe_tipoin	=	:li_Tipo
AND   inpe_numero	=	:al_numero;

IF IsNull(ll_Numero) OR ll_Numero = 0 THEN
	//MessageBox("Cuidado","Inpeccion No Existe")
	RETURN False
ELSE
	MessageBox("Cuidado","Inspección ya tiene detalle, proceda por mantención")
	RETURN True
END IF
	
end function

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
Long numero
Integer li_planta, li_movto, li_tipoin

IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
			
		dw_1.ResetUpdate()
		em_numero.text = istr_mant.argumento[4]
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

//Numero = Long(istr_mant.argumento[4])
//li_planta = Integer(istr_mant.argumento[2])
//li_tipoin = Integer(istr_mant.argumento[3])
//
//IF li_tipoin = 1 THEN
//	li_movto = 5
//ELSE
//	li_movto = 6
//END IF	
//
///*actualiza numero actual en correlativos */
//update dbo.CORRELMOVIMIENTOS set
//COMO_ACTUAl = :numero
//where plde_codigo = :li_planta
//And	como_tipomv = :li_movto;
//
RETURN lb_Retorno
end function

on w_mant_mues_inspecpalenc_nuevo.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.ddlb_tipocond=create ddlb_tipocond
this.st_4=create st_4
this.st_3=create st_3
this.em_numero=create em_numero
this.cb_detalle=create cb_detalle
this.st_5=create st_5
this.st_6=create st_6
this.em_fecha=create em_fecha
this.st_7=create st_7
this.sle_solic=create sle_solic
this.st_22=create st_22
this.em_desde=create em_desde
this.st_23=create st_23
this.em_hasta=create em_hasta
this.cbx_fumigacion=create cbx_fumigacion
this.st_9=create st_9
this.sle_observacion=create sle_observacion
this.cbx_facturable=create cbx_facturable
this.st_10=create st_10
this.uo_selplantas=create uo_selplantas
this.uo_selcliente=create uo_selcliente
this.uo_seldestino=create uo_seldestino
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.ddlb_tipocond
this.Control[iCurrent+4]=this.st_4
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.em_numero
this.Control[iCurrent+7]=this.cb_detalle
this.Control[iCurrent+8]=this.st_5
this.Control[iCurrent+9]=this.st_6
this.Control[iCurrent+10]=this.em_fecha
this.Control[iCurrent+11]=this.st_7
this.Control[iCurrent+12]=this.sle_solic
this.Control[iCurrent+13]=this.st_22
this.Control[iCurrent+14]=this.em_desde
this.Control[iCurrent+15]=this.st_23
this.Control[iCurrent+16]=this.em_hasta
this.Control[iCurrent+17]=this.cbx_fumigacion
this.Control[iCurrent+18]=this.st_9
this.Control[iCurrent+19]=this.sle_observacion
this.Control[iCurrent+20]=this.cbx_facturable
this.Control[iCurrent+21]=this.st_10
this.Control[iCurrent+22]=this.uo_selplantas
this.Control[iCurrent+23]=this.uo_selcliente
this.Control[iCurrent+24]=this.uo_seldestino
end on

on w_mant_mues_inspecpalenc_nuevo.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.ddlb_tipocond)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.em_numero)
destroy(this.cb_detalle)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.em_fecha)
destroy(this.st_7)
destroy(this.sle_solic)
destroy(this.st_22)
destroy(this.em_desde)
destroy(this.st_23)
destroy(this.em_hasta)
destroy(this.cbx_fumigacion)
destroy(this.st_9)
destroy(this.sle_observacion)
destroy(this.cbx_facturable)
destroy(this.st_10)
destroy(this.uo_selplantas)
destroy(this.uo_selcliente)
destroy(this.uo_seldestino)
end on

event open;call super::open;Boolean	lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelDestino.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelDestino.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gi_CodPlanta)
	
	dw_1.GetChild("espe_codigo",dwc_especie)
	dw_1.GetChild("vari_codigo",dwc_variedad)
	dw_1.GetChild("emba_codigo",dwc_embalaje)
	dw_1.GetChild("tpem_codigo",dwc_tipopalemb)
	
	dwc_especie.SetTransObject(Sqlca)
	dwc_variedad.SetTransObject(Sqlca)
	dwc_embalaje.SetTransObject(Sqlca)
	dwc_tipopalemb.SetTransObject(Sqlca)
	
	dwc_especie.Retrieve()						//Especies del Cliente
	dwc_variedad.Retrieve(0)					//Variedades del Cliente y la Especie
	dwc_embalaje.Retrieve(gi_codexport)						//Embalajes del Cliente
	dwc_tipopalemb.Retrieve(gi_codexport,'Z')				//Tipos de Pallet por Embalajes del Cliente
	
	wf_opciontodas()
	
	em_fecha.Text	= String(Today(),'dd/mm/yyyy')
	
	istr_mant.argumento[3]	= 	'1'											//Tipo de Condición
	istr_mant.argumento[4]	= 	'0'											//Número de Solicitud
	istr_mant.argumento[6]	= 	String(Today(),'dd/mm/yyyy')			//Fecha de Proceso
	istr_mant.argumento[7]	=  ''                            						//Solicitante
	istr_mant.argumento[8]	=	String(RelativeDate(Today(), -365)) //Fecha Packing Inicio
	istr_mant.argumento[9]	=	String(Today(),'dd/mm/yyyy')  		//Fecha Packing Término
	
	em_desde.text				=	String(RelativeDate(Today(), -365))
	em_hasta.text				=	String(Today(),'dd/mm/yyyy')
	
	ddlb_tipocond.SelectItem(1)
	
	buscar	= "Destino:Ndest_codigo,Especie:Nespe_codigo,Variedad:Nvari_codigo,Embalaje:Semba_codigo,Cajas x Pallet:Ninpe_cajpal"
	ordenar	= "Destino:dest_codigo,Especie:espe_codigo,Variedad:vari_codigo,Embalaje:emba_codigo,Cajas x Pallet:inpe_cajpal"
End If
end event

event ue_recuperadatos;Long		ll_fila, respuesta
Integer 	li_Null
SetNull(li_Null)

DO
	ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),&
									 uo_SelCliente.Codigo, uo_SelPlantas.Codigo)
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		//Incorporar la recuperación del detalle generado para esta solicitud
		pb_lectura.Enabled		=	True
		cb_detalle.Enabled		= 	True
		ddlb_tipocond.Enabled	=	False
		em_numero.Enabled		=	False
		em_fecha.Enabled			=	False	
		sle_solic.Enabled       		=  False
		em_desde.Enabled		=	False
		em_hasta.Enabled			=	False
		//Asigna a Destinos el Destino correspondiente
		//istr_mant.argumento[5]	=	String(dw_1.Object.dest_codigo[1])
		//dw_destino.SetItem(1,"dest_codigo",Integer(istr_mant.argumento[5]))
		//dw_destino.Enabled		=	False

		//Asigna Fecha de Inspección / Re-Inspección
		istr_mant.argumento[6]	=	String(dw_1.Object.inpe_fechai[1])
		em_fecha.Text				=	istr_mant.argumento[6]
		em_fecha.Enabled			=	False
		uo_SelDestino.Inicia(dw_1.Object.dest_codigo[1])
		
		IF uo_SelDestino.Fumiga = 1 THEN
			cbx_fumigacion.Enabled = True
		ELSE	
			cbx_fumigacion.Enabled = False
		END IF	
			
		sle_solic.text          		= dw_1.Object.inpe_solnom[1]
		istr_mant.argumento[7]  = dw_1.Object.inpe_solnom[1]
		//
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_lectura.Enabled		=	False
		sle_solic.Enabled       =  True
		sle_solic.text          = ''
		em_desde.Enabled			=	True
		em_hasta.Enabled			=	True
		//pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo;call super::ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True
istr_mant.dw		= dw_1
istr_mant.argumento[1] = String(uo_SelCliente.Codigo)
istr_mant.argumento[2] = String(uo_SelPlantas.Codigo)
istr_mant.argumento[4] = em_numero.Text//String(buscainspeccion(Integer(istr_mant.argumento[2])))
istr_mant.argumento[5] = String(uo_SelDestino.Codigo)

IF Long(Istr_mant.argumento[4]) = 0 OR Isnull(Istr_mant.argumento[4]) THEN
	MessageBox("Advertencia","Falta Ingresar Número Inspección.")
	Return
END IF	

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "SOLICITUD DE INSPECCIÓN - RE-INSPECCIÓN"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_inspecpalenc"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),&
								  uo_SelCliente.Codigo, uo_SelPlantas.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("desde.text = '" + istr_mant.argumento[8] + "'")
	vinf.dw_1.Modify("hasta.text = '" + istr_mant.argumento[9] + "'")
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_borrar;call super::ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra		= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

 IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
		dw_1.SelectRow(il_fila,True)
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_fila, ll_filas, ll_numero
Integer	li_secuencia, li_tipoin, li_factur
Date		ld_desde, ld_hasta

li_tipoin	=	Integer(istr_mant.argumento[3])
ll_numero	=	Long(istr_mant.argumento[4])
ld_desde		=	Date(istr_mant.argumento[8])
ld_hasta		=	Date(istr_mant.argumento[9])


IF cbx_facturable.Checked THEN
	li_factur = 1
ELSe
	li_factur = 0
END IF

SELECT	Max(inpe_secuen)
	INTO	:li_secuencia
	FROM	dbo.inspecpalenc
	WHERE	inpe_tipoin = :li_tipoin
	AND	inpe_numero	=	:ll_numero
	AND	clie_codigo	=	:uo_SelCliente.Codigo
	AND	plde_codigo	=	:uo_SelPlantas.Codigo;

IF Isnull(li_secuencia) THEN li_secuencia = 0

ll_filas	=	dw_1.RowCount()

FOR ll_fila = 1 TO ll_filas
	IF Isnull(dw_1.Object.inpe_secuen[ll_fila]) or dw_1.Object.inpe_secuen[ll_fila] = 0 THEN
		li_secuencia ++
		dw_1.Object.inpe_secuen[ll_fila] = li_secuencia
		dw_1.Object.inpe_fecini[ll_fila]	= ld_desde
		dw_1.Object.inpe_fecter[ll_fila]= ld_hasta
		dw_1.Object.inpe_estado[ll_fila] = 5
		dw_1.Object.inpe_factur[ll_fila] = li_factur
	END IF
NEXT
end event

event ue_guardar;call super::ue_guardar;Str_mant	lstr_mant
Integer	li_tipoin,li_cliente,li_planta, li_factur
Long		ll_numero
Date		ld_desde, ld_hasta

IF cbx_facturable.Checked THEN
	li_factur = 1
ELSe
	li_factur = 0
END IF

IF Message.DoubleParm = 0 THEN
	//Abrir la siguiente ventana que muestra el detalle de los pallets 
	// y en donde se puede eliminar o insertar uno nuevo (pallet o pucho).
	li_tipoin	=	Integer(istr_mant.argumento[3])
	ll_numero	=	Long(istr_mant.argumento[4])
	li_cliente	=	Integer(istr_mant.argumento[1])
	li_planta	=	Integer(istr_mant.argumento[2])
	ld_desde		=	Date(istr_mant.argumento[8])
	ld_hasta		=	Date(istr_mant.argumento[9])
	
	lstr_mant.argumento[1]	=	istr_mant.argumento[1]
	lstr_mant.argumento[2]	=	istr_mant.argumento[2]
	lstr_mant.argumento[3]	=	istr_mant.argumento[3]
	lstr_mant.argumento[4]	=	istr_mant.argumento[4]
	lstr_mant.argumento[5]	=	istr_mant.argumento[5]
	lstr_mant.argumento[6]	=	istr_mant.argumento[6]
	lstr_mant.argumento[8]	=	istr_mant.argumento[8]
	lstr_mant.argumento[9]	=	istr_mant.argumento[9]
//	lstr_mant.argumento[9]	=  istr_mant.argumento[15]
	
	
	lstr_mant.argumento[30]	=  String(li_factur)
	

	DECLARE Genera_Detalle PROCEDURE FOR dbo.FProcGen_DetalleInspec  
         @tipoin  = :li_tipoin,   
         @numero  = :ll_numero,   
         @cliente = :li_cliente,   
         @planta  = :li_planta, 
			@desde   = :ld_desde,
			@hasta   = :ld_hasta ;

	EXECUTE Genera_Detalle;
	
	IF SQLCA.SQLCode = -1 THEN
		f_errorbasedatos(sqlca,"Generación Detalle Inpección Pallets")
		RETURN
	ELSE
		cb_detalle.Enabled	= True
		istr_mant.argumento[10] = '1'
		lstr_mant.argumento[10] = istr_mant.argumento[10] 
		
		IF uo_SelDestino.Fumiga= 1 THEN
			lstr_mant.argumento[15] = '1'
		ELSE	
			lstr_mant.argumento[15] = '0'
		END IF
				
		OpenWithParm(/*w_mant_mues_inspecpaldet*/w_mant_mues_inspecpaldet_directo_nuevo,lstr_mant)
	END IF
END IF
end event

event resize;call super::resize;cb_detalle.x	=	pb_imprimir.x
cb_detalle.y	=	pb_imprimir.y + 255

st_10.width	=	st_encabe.width
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_inspecpalenc_nuevo
integer x = 55
integer y = 748
integer width = 2912
integer height = 1100
integer taborder = 110
string dataobject = "dw_mues_inspecpalenc"
boolean hscrollbar = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_inspecpalenc_nuevo
integer x = 55
integer y = 24
integer width = 2912
integer height = 592
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_inspecpalenc_nuevo
integer x = 3314
integer y = 144
integer taborder = 120
end type

event pb_lectura::clicked;call super::clicked;uo_SelCliente.Bloquear(True)
uo_SelPlantas.Bloquear(True)

IF Integer(istr_mant.argumento[3]) = 0 THEN
	MessageBox("Atención","Debe seleccionar el Tipo de Condición previamente.")
	RETURN
ELSEIF Long(istr_mant.argumento[4]) = 0 THEN
	MessageBox("Atención","Debe ingresar el Número de Solicitud previamente.")
	RETURN
ELSE
	Call SUPER::Clicked
	IF dw_1.RowCount() = 0 THEN
		IF NOT IsDate(em_fecha.Text) THEN
			MessageBox("Atención","Debe seleccionar la Fecha de Inspección / Re-Inspección previamente.")
			RETURN
		END IF
	END IF
	ddlb_tipocond.Enabled	=	False
	em_numero.Enabled			=	False
	em_fecha.Enabled			=	False
	em_desde.Enabled			=	False
	em_hasta.Enabled			=	False
END IF
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_inspecpalenc_nuevo
integer x = 3310
integer y = 440
integer taborder = 170
end type

event pb_nuevo::clicked;call super::clicked;Integer li_Null
Setnull(li_null)

uo_SelCliente.Bloquear(False)
uo_SelPlantas.Bloquear(False)
uo_SelDestino.Bloquear(False)

istr_mant.argumento[4]	= 	'0'									//Número de Solicitud
istr_mant.argumento[6]	= 	String(Today(),'dd/mm/yyyy')	//Número de Solicitud
em_numero.Text			=  '0'
em_fecha.Text				=	String(Today(),'dd/mm/yyyy')

ddlb_tipocond.Enabled	=	True
em_numero.Enabled		=	True
em_fecha.Enabled			=	True
cb_detalle.Enabled			=	False
em_desde.Enabled		=	True
em_hasta.Enabled			=	True
pb_insertar.Enabled 		=  False
cbx_fumigacion.Enabled 	= False

sle_solic.Enabled       =  True
sle_solic.text          	= ''

pb_lectura.Enabled		=	False
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_inspecpalenc_nuevo
integer x = 3310
integer y = 620
integer taborder = 130
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_inspecpalenc_nuevo
integer x = 3310
integer y = 800
integer taborder = 140
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_inspecpalenc_nuevo
integer x = 3310
integer y = 980
integer taborder = 150
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_inspecpalenc_nuevo
integer x = 3310
integer y = 1160
integer taborder = 160
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_inspecpalenc_nuevo
integer x = 3310
integer y = 1620
integer taborder = 180
end type

type st_1 from statictext within w_mant_mues_inspecpalenc_nuevo
integer x = 87
integer y = 80
integer width = 315
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

type st_2 from statictext within w_mant_mues_inspecpalenc_nuevo
integer x = 87
integer y = 192
integer width = 315
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

type ddlb_tipocond from dropdownlistbox within w_mant_mues_inspecpalenc_nuevo
integer x = 544
integer y = 288
integer width = 814
integer height = 292
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
string text = "none"
boolean sorted = false
integer limit = 1
string item[] = {"Inspección","",""}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;istr_mant.argumento[3]	=	String(index)
end event

type st_4 from statictext within w_mant_mues_inspecpalenc_nuevo
integer x = 87
integer y = 300
integer width = 315
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Condición"
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_mues_inspecpalenc_nuevo
integer x = 1783
integer y = 192
integer width = 251
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
string text = "Número"
boolean focusrectangle = false
end type

type em_numero from editmask within w_mant_mues_inspecpalenc_nuevo
integer x = 2053
integer y = 180
integer width = 430
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;IF ExisteInspeccion(Long(This.Text)) = False THEN
//	em_numero.Text = ''
//	istr_mant.argumento[4] = em_numero.Text//= String(buscainspeccion(Integer(istr_mant.argumento[2])))
//	Parent.TriggerEvent("ue_recuperadatos")
//istr_mant.argumento[3] = em_numero.Text
ELSE	
	em_numero.Text = ''
	em_numero.SetFocus()
END IF

end event

type cb_detalle from commandbutton within w_mant_mues_inspecpalenc_nuevo
integer x = 3310
integer y = 1432
integer width = 302
integer height = 132
integer taborder = 190
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Detalle"
end type

event clicked;str_mant	lstr_mant

lstr_mant.argumento[1]	=	istr_mant.argumento[1]
lstr_mant.argumento[2]	=	istr_mant.argumento[2]
lstr_mant.argumento[3]	=	istr_mant.argumento[3]
lstr_mant.argumento[4]	=	istr_mant.argumento[4]
lstr_mant.argumento[5]	=	istr_mant.argumento[5]
lstr_mant.argumento[6]	=	istr_mant.argumento[6]
lstr_mant.argumento[7]	=	istr_mant.argumento[7]
lstr_mant.argumento[8]	=	istr_mant.argumento[8]
lstr_mant.argumento[9]	=	istr_mant.argumento[9]

istr_mant.argumento[10] = '1'
lstr_mant.argumento[10] = istr_mant.argumento[10] 

IF uo_SelDestino.Fumiga = 1 THEN
	lstr_mant.argumento[15] = '1'
ELSE	
	lstr_mant.argumento[15] = '0'
END IF	

OpenWithParm(/*w_mant_mues_inspecpaldet*/w_mant_mues_inspecpaldet_directo_nuevo,lstr_mant)

end event

type st_5 from statictext within w_mant_mues_inspecpalenc_nuevo
integer x = 1376
integer y = 300
integer width = 645
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
string text = "Destino para Asignar"
boolean focusrectangle = false
end type

type st_6 from statictext within w_mant_mues_inspecpalenc_nuevo
integer x = 1783
integer y = 80
integer width = 251
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
string text = "Fecha"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_inspecpalenc_nuevo
integer x = 2053
integer y = 64
integer width = 526
integer height = 96
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

event modified;IF Not f_validafechatempo(date(this.Text)) THEN
	This.Text = ''
	This.SetFocus()
END IF

istr_mant.argumento[6]	=	This.Text
end event

type st_7 from statictext within w_mant_mues_inspecpalenc_nuevo
integer x = 87
integer y = 400
integer width = 402
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Solicitante"
boolean focusrectangle = false
end type

type sle_solic from singlelineedit within w_mant_mues_inspecpalenc_nuevo
integer x = 544
integer y = 388
integer width = 2373
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
integer limit = 60
borderstyle borderstyle = stylelowered!
end type

event modified;istr_mant.argumento[7]	=	This.Text
end event

type st_22 from statictext within w_mant_mues_inspecpalenc_nuevo
integer x = 101
integer y = 652
integer width = 622
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
string text = "Fecha Packing Inicio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_desde from editmask within w_mant_mues_inspecpalenc_nuevo
integer x = 745
integer y = 632
integer width = 421
integer height = 96
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[8]	=	This.Text
end event

type st_23 from statictext within w_mant_mues_inspecpalenc_nuevo
integer x = 1175
integer y = 652
integer width = 297
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
string text = "Término"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_mant_mues_inspecpalenc_nuevo
integer x = 1454
integer y = 632
integer width = 421
integer height = 96
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[9]	=	This.Text


end event

type cbx_fumigacion from checkbox within w_mant_mues_inspecpalenc_nuevo
boolean visible = false
integer x = 2295
integer y = 1816
integer width = 475
integer height = 44
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Fumigación"
end type

type st_9 from statictext within w_mant_mues_inspecpalenc_nuevo
integer x = 87
integer y = 512
integer width = 439
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Observaciones"
boolean focusrectangle = false
end type

type sle_observacion from singlelineedit within w_mant_mues_inspecpalenc_nuevo
integer x = 544
integer y = 496
integer width = 2373
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
integer limit = 200
borderstyle borderstyle = stylelowered!
end type

event modified;istr_mant.argumento[15]	=	This.Text
end event

type cbx_facturable from checkbox within w_mant_mues_inspecpalenc_nuevo
integer x = 2048
integer y = 640
integer width = 681
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
string text = "Proceso Facturable  "
boolean checked = true
end type

type st_10 from statictext within w_mant_mues_inspecpalenc_nuevo
integer x = 55
integer y = 616
integer width = 2912
integer height = 128
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selplantas from uo_seleccion_plantas within w_mant_mues_inspecpalenc_nuevo
integer x = 544
integer y = 176
integer height = 96
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_inspecpalenc_nuevo
integer x = 544
integer y = 64
integer height = 96
integer taborder = 10
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_seldestino from uo_seleccion_destinos within w_mant_mues_inspecpalenc_nuevo
event destroy ( )
integer x = 2021
integer y = 276
integer height = 96
integer taborder = 60
boolean bringtotop = true
end type

on uo_seldestino.destroy
call uo_seleccion_destinos::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

pb_insertar.Enabled = True

end event

