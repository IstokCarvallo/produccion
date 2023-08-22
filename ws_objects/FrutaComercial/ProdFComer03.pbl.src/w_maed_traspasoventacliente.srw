$PBExportHeader$w_maed_traspasoventacliente.srw
$PBExportComments$Recepción de Fruta Granel de Huerto
forward
global type w_maed_traspasoventacliente from w_mant_encab_deta_csd
end type
type tab_1 from tab within w_maed_traspasoventacliente
end type
type tp_1 from userobject within tab_1
end type
type dw_detafruta from uo_dw within tp_1
end type
type tp_1 from userobject within tab_1
dw_detafruta dw_detafruta
end type
type tp_2 from userobject within tab_1
end type
type dw_envrec from uo_dw within tp_2
end type
type tp_2 from userobject within tab_1
dw_envrec dw_envrec
end type
type tp_3 from userobject within tab_1
end type
type dw_envdes from uo_dw within tp_3
end type
type tp_3 from userobject within tab_1
dw_envdes dw_envdes
end type
type tab_1 from tab within w_maed_traspasoventacliente
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
end type
type str_productores_envases from structure within w_maed_traspasoventacliente
end type
end forward

type str_productores_envases from structure
	integer		productor[]
	long		guiasii[]
	integer		tipomovto[]
	integer		Numero[]
end type

global type w_maed_traspasoventacliente from w_mant_encab_deta_csd
integer height = 1992
string title = "DESPACHO RETIRO VENTAS"
string menuname = ""
tab_1 tab_1
end type
global w_maed_traspasoventacliente w_maed_traspasoventacliente

type variables
w_mant_deta_movtofrutcomer_desp_cliente	iw_mantencion_1
w_mant_deta_movtoenvadeta_despa_comer	iw_mantencion_2

DataWindowChild	idwc_Variedad,idwc_Predio,&
						idwc_Camara, idwc_cliente
DataWindow			dw_3,dw_5,dw_7

uo_especie			iuo_Especie
uo_pesoestanespe	iuo_PesoEstanEspe
uo_fechaMovto		iuo_FechaMovto

uo_tipomovtofruta		iuo_TipoMovtoFruta
uo_tipomovtofruta		iuo_TipoMovtoEnva


Boolean	ib_AutoCommit
DateTime	idt_FechaSistema

Private:
str_Productores_Envases	wstr_Prod_Enva
end variables

forward prototypes
public subroutine productoreslotes (ref string productores[])
protected function integer wf_modifica ()
public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, integer al_numero)
public subroutine determina_productoresenvase (integer ai_tipomovto)
public subroutine habilitaencab (boolean habilita)
public function boolean existeorden (long al_orden, integer ai_tipo)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine habilitagrabacion (string as_columna)
public subroutine habilitaingreso (string as_columna)
public subroutine buscaorden ()
end prototypes

public subroutine productoreslotes (ref string productores[]);Long		ll_Fila
Integer	li_Secuencia
String	ls_productor, ls_ProdAnt, ls_Nula[]

Productores	=	ls_Nula

FOR ll_Fila = 1 TO dw_3.RowCount()
	ls_Productor	=	dw_3.Object.prod_rut[ll_Fila]
	
	IF ls_Productor <> ls_ProdAnt THEN
		li_Secuencia ++
		Productores[li_Secuencia]	=	ls_Productor
	
		ls_ProdAnt	=	ls_Productor
	END IF
NEXT

RETURN
end subroutine

protected function integer wf_modifica ();IF dw_3.AcceptText() = -1 THEN RETURN -1
IF dw_5.AcceptText() = -1 THEN RETURN -1
IF dw_7.AcceptText() = -1 THEN RETURN -1
IF dw_2.AcceptText() = -1 THEN RETURN -1
IF (dw_1.ModifiedCount() + dw_1.DeletedCount()) > 0 THEN RETURN 0
IF (dw_3.ModifiedCount() + dw_3.DeletedCount()) > 0 THEN RETURN 0
IF (dw_5.ModifiedCount() + dw_5.DeletedCount()) > 0 THEN RETURN 0
IF (dw_7.ModifiedCount() + dw_7.DeletedCount()) > 0 THEN RETURN 0
IF dw_2.ModifiedCount() > 0 THEN RETURN 0

RETURN 1
end function

public function boolean existerecepcion (integer ai_planta, integer ai_tipomovto, integer al_numero);Long		ll_Numero
Boolean	lb_Retorno = True

SELECT	mfco_numero
	INTO	:ll_Numero
	FROM	dba.spro_movtofrutacomenca
	WHERE	plde_codigo	=	:ai_Planta
	AND	tpmv_codigo	=	:ai_TipoMovto
	AND	mfco_numero	=	:al_Numero ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Recepción Fruta Granel")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	istr_mant.Argumento[3]	=	String(al_Numero)
	This.TriggerEvent("ue_recuperadatos")
ELSE
	MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine determina_productoresenvase (integer ai_tipomovto);str_Productores_Envases	lstr_Prod_Enva
DataWindow	ldw_envase
Long			ll_Fila, ll_GuiaSII, ll_Fila_Lote,ll_Productor
Integer		li_ProdAnt, li_Secuencia

//Inicializa la Estructura de Instancia
wstr_Prod_Enva	= lstr_Prod_Enva

IF ai_TipoMovto = 64 THEN
	ldw_envase	=	dw_5
ELSE
	ldw_envase	=	dw_7
END IF

FOR ll_Fila	=	1 TO ldw_Envase.RowCount()
	ll_Productor	=	ldw_Envase.Object.prod_codigo[ll_Fila]
	
	IF ll_Productor <> li_ProdAnt THEN
		li_ProdAnt	=	ll_Productor
		li_Secuencia ++
		
//		IF ai_TipoMovto = 64 THEN
//			ll_Fila_Lote	=	dw_3.Find("prod_codigo = "+String(li_Productor),1,dw_3.RowCount())
//			
//			IF ll_Fila_Lote > 0 THEN
//				ll_GuiaSII	=	dw_3.Object.lote_guisii[ll_Fila_Lote]
//			ELSE
				ll_GuiaSII	=	0
//			END IF
//		END IF
		
		wstr_Prod_Enva.Productor[li_Secuencia]	=	ll_Productor
		wstr_Prod_Enva.GuiaSII[li_Secuencia]	=	ll_GuiaSII
		wstr_Prod_Enva.TipoMovto[li_Secuencia]	=	64
	END IF
NEXT

RETURN
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.mfco_numero.Protect				=	0
	dw_2.Object.mfco_numero.BackGround.Color	=	RGB(255,255,255)
	
	dw_2.Object.mfco_docrel.Protect				=	0
	dw_2.Object.mfco_docrel.BackGround.Color	=	RGB(255,255,255)
	
	dw_2.Object.tran_codigo.Protect				=	0
	dw_2.Object.tran_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.cami_patent.Protect				=	0
	dw_2.Object.cami_patent.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.cami_patcar.Protect				=	0
	dw_2.Object.cami_patcar.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.mfco_rutcho.Protect				=	0
	dw_2.Object.mfco_rutcho.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.mfco_chofer.Protect				=	0
	dw_2.Object.mfco_chofer.BackGround.Color	=	RGB(255,255,255)
	dw_2.Object.mfco_totbul.Protect				=	0
	dw_2.Object.mfco_totbul.BackGround.Color	=	RGB(255,255,255)	

	dw_2.Object.mfco_tkbent.Protect				=	0
	dw_2.Object.mfco_tkbent.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.mfco_tkbenc.Protect				=	0
	dw_2.Object.mfco_tkbenc.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.mfco_fecmov.Protect				=	0
	dw_2.Object.mfco_fecmov.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.mfco_horaen.Protect				=	0
	dw_2.Object.mfco_horaen.BackGround.Color	=	RGB(255,255,255)	
	dw_2.Object.mfco_totbul.Protect				=	1
	dw_2.Object.mfco_totbul.BackGround.Color	=	RGB(166,180,210)	

  	dw_2.Object.b_orden.visible					=  1
   dw_2.Object.buscacamion.visible				=  1

	dw_2.Object.mfco_tkbsal.Protect				=	1
	dw_2.Object.mfco_tkbsal.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.mfco_tkbsac.Protect				=	1
	dw_2.Object.mfco_tkbsac.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.mfco_horasa.Protect				=	1
	dw_2.Object.mfco_horasa.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.fecha_sal.Protect					=	1
	dw_2.Object.fecha_sal.BackGround.Color		=	RGB(166,180,210)
	tab_1.tp_3.Enabled								=	False
	

ELSE
	dw_2.Object.mfco_numero.Protect				=	1
	dw_2.Object.mfco_numero.BackGround.Color	=	RGB(166,180,210)
	
	dw_2.Object.mfco_docrel.Protect				=	1
	dw_2.Object.mfco_docrel.BackGround.Color	=	RGB(166,180,210)
	
	dw_2.Object.tran_codigo.Protect				=	1
	dw_2.Object.tran_codigo.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.cami_patent.Protect				=	1
	dw_2.Object.cami_patent.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.cami_patcar.Protect				=	1
	dw_2.Object.cami_patcar.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.mfco_rutcho.Protect				=	1
	dw_2.Object.mfco_rutcho.BackGround.Color	=	RGB(166,180,210)
	dw_2.Object.mfco_chofer.Protect				=	1
	dw_2.Object.mfco_chofer.BackGround.Color	=	RGB(166,180,210)
  	dw_2.Object.b_orden.visible					=  0
   dw_2.Object.buscacamion.visible				=  0

	IF dw_2.Object.mfco_estmov[1] = 3 THEN
		dw_2.Object.mfco_tkbent.Protect				=	1
		dw_2.Object.mfco_tkbent.BackGround.Color	=	RGB(166,180,210)	
		dw_2.Object.mfco_tkbenc.Protect				=	1
		dw_2.Object.mfco_tkbenc.BackGround.Color	=	RGB(166,180,210)	
		dw_2.Object.mfco_fecmov.Protect				=	1
		dw_2.Object.mfco_fecmov.BackGround.Color	=	RGB(166,180,210)	
		dw_2.Object.mfco_horaen.Protect				=	1
		dw_2.Object.mfco_horaen.BackGround.Color	=	RGB(166,180,210)	
		dw_2.Object.mfco_totbul.Protect				=	1
		dw_2.Object.mfco_totbul.BackGround.Color	=	RGB(166,180,210)	
	END IF		
END IF

end subroutine

public function boolean existeorden (long al_orden, integer ai_tipo);Integer	li_Contador, li_planta
Long		ll_Numero
String   ls_rut, ls_obser
Boolean	lb_Retorno = True

li_planta = dw_2.Object.plde_codigo[1]

SELECT	re.oret_numero, vt.clpr_rut, vt.odfc_observ
	INTO	:ll_Numero, :ls_rut, :ls_obser
	FROM	dba.spro_ordenventacomenca vt, dba.spro_ordenretiroventaenc re
	WHERE	re.plde_codigo	=	:li_Planta
	AND	re.oret_numero	=	:al_orden
	AND   re.plde_codigo =  vt.plde_codigo
	AND   re.odfc_numero =  vt.odfc_numero;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Orden de Retiro")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 0 THEN
	dw_2.SetItem(1,"clpr_rut",ls_rut)
	dw_2.SetItem(1,"mfco_observ",ls_obser)
ELSE
	MessageBox("Atención","Número de Correlativo No ha sido generado. Ingrese Otro.")
	lb_Retorno	=	False
END IF

RETURN lb_Retorno

end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

IF NOT Borrando THEN
	IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		dw_2.SetItem(1,"mfco_usuari",gstr_us.nombre)
		dw_2.SetItem(1,"mfco_comput",gstr_us.computador)
		dw_2.SetItem(1,"mfco_horacr",F_FechaHora())
	ELSEIF dw_2.GetItemStatus(1, 0, Primary!) = DataModified! THEN
		dw_2.SetItem(1,"mfco_usumod",gstr_us.nombre)
		dw_2.SetItem(1,"mfco_commod",gstr_us.computador)
		dw_2.SetItem(1,"mfco_horact",F_FechaHora())
		
		IF dw_3.DeletedCount() > 0 THEN Borrando = True
	END IF
END IF

IF Borrando THEN
	IF dw_5.Update(True, False) = 1 THEN 					//Detalle Envase Recep
		IF dw_7.Update(True, False) = 1 THEN				//Detalle Envase Retirados
			IF dw_1.Update(True, False) = 1 THEN			//Encabezado de Envases
				IF dw_3.Update(True,False) = 1 THEN			//Detalle Movimiento
					IF dw_2.Update(True,False) = 1 THEN		//Encabezado Movimiento
						Commit;
							
						IF sqlca.SQLCode <> 0 THEN
							F_ErrorBaseDatos(sqlca, This.Title)
							RollBack;
						ELSE
							lb_Retorno	=	True
									
							dw_7.ResetUpdate()
							dw_5.ResetUpdate()
							dw_3.ResetUpdate()
							dw_2.ResetUpdate()
							dw_1.ResetUpdate()
						END IF
					ELSE
						F_ErrorBaseDatos(sqlca, This.Title)
						RollBack;
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF dw_2.Update(True, False) = 1 THEN		 				//Encabezado Movimiento
		IF dw_3.Update(True, False) = 1 THEN					//Detalle de Movimiento
			IF dw_1.Update(True, False) = 1 THEN				//Encabezado de Envases
				IF dw_5.Update(True,False) = 1 THEN				//Envases Recepcionados
					IF dw_7.Update(True,False) = 1 THEN			//Envases Retirados
						Commit;
						
						IF sqlca.SQLCode <> 0 THEN
							F_ErrorBaseDatos(sqlca, This.Title)
						ELSE
							lb_Retorno	=	True
							
							dw_7.ResetUpdate()
							dw_5.ResetUpdate()
							dw_3.ResetUpdate()
							dw_2.ResetUpdate()
							dw_1.ResetUpdate()
						END IF
					ELSE
						F_ErrorBaseDatos(sqlca, This.Title)
						RollBack;
					END IF
				ELSE
					F_ErrorBaseDatos(sqlca, This.Title)
					RollBack;
				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	ib_AutoCommit

RETURN lb_Retorno
end function

public subroutine habilitagrabacion (string as_columna);Boolean	lb_Estado = True
Date		ldt_Fecha

IF as_Columna <> "fecha_sal" AND &
	(dw_2.Object.fecha_sal[1] = ldt_Fecha OR IsNull(dw_2.Object.fecha_sal[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "mfco_horasa" AND &
	(dw_2.Object.mfco_horasa[1] = ldt_Fecha OR IsNull(dw_2.Object.mfco_horasa[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "mfco_tkbsal" AND &
	(dw_2.Object.mfco_tkbent[1] = 0 OR IsNull(dw_2.Object.mfco_tkbent[1])) THEN
	lb_Estado = False
END IF

pb_Grabar.Enabled	=	lb_Estado
end subroutine

public subroutine habilitaingreso (string as_columna);Boolean	lb_Estado = True
Date		ldt_Fecha

dw_2.Accepttext()

IF as_Columna <> "mfco_fecmov" AND &
	(dw_2.Object.mfco_fecmov[1] = ldt_Fecha OR IsNull(dw_2.Object.mfco_fecmov[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "mfco_horaen" AND &
	(dw_2.Object.mfco_horaen[1] = ldt_Fecha OR IsNull(dw_2.Object.mfco_horaen[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "tran_codigo" AND &
	IsNull(dw_2.Object.tran_codigo[1]) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "cami_patent" AND &
	(dw_2.Object.cami_patent[1] = "" OR IsNull(dw_2.Object.cami_patent[1])) THEN
	lb_Estado = False
END IF
	
IF as_Columna <> "mfco_chofer" AND &
	(dw_2.Object.mfco_chofer[1] = "" OR IsNull(dw_2.Object.mfco_chofer[1])) THEN
	lb_Estado = False
END IF

IF as_Columna <> "mfco_tkbent" AND &
	(dw_2.Object.mfco_tkbent[1] = 0 OR IsNull(dw_2.Object.mfco_tkbent[1])) THEN
	lb_Estado = False
END IF

IF dw_2.Object.mfco_estmov[1]	<>	1 THEN
	tab_1.tp_1.Enabled	=	lb_Estado
END IF

tab_1.tp_2.Enabled	=	lb_Estado
IF dw_2.Object.mfco_estmov[1] <> 3 THEN
   pb_ins_det.Enabled	=	lb_Estado
END IF	

pb_grabar.Enabled		=	lb_Estado

end subroutine

public subroutine buscaorden ();Long           ll_Nula
Str_Busqueda	lstr_busq

SetNull(ll_Nula)

lstr_busq.argum[1] = String(gstr_param.plde_codigo)
lstr_busq.argum[2] = ''								// Tipo de Movimiento
lstr_busq.argum[3] = ''									// 

OpenWithParm(w_busc_ordenretiroventa, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[2] <> "" THEN
	IF Not existeorden(long(lstr_busq.argum[2]),integer(lstr_busq.argum[1])) THEN
		dw_2.SetItem(1, "mfco_docrel", ll_Nula)
		RETURN
	ELSE
		dw_2.SetItem(1, "mfco_docrel", long(lstr_busq.argum[2]))
	END IF
ELSE
	dw_2.SetItem(1, "mfco_docrel", ll_Nula)
	RETURN

END IF
end subroutine

on w_maed_traspasoventacliente.create
int iCurrent
call super::create
this.tab_1=create tab_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_1
end on

on w_maed_traspasoventacliente.destroy
call super::destroy
destroy(this.tab_1)
end on

event open;/* Argumentos
istr_mant.argumento[1] = Planta por Defecto 
istr_mant.argumento[2] = 
istr_mant.argumento[3] = 
istr_mant.argumento[4] = 
istr_mant.argumento[5] = 
istr_mant.argumento[6] = 
istr_mant.argumento[7] = 
istr_mant.argumento[7] = 
istr_mant.argumento[9] = 
istr_mant.argumento[10] =
*/

x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

istr_mant.argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[2]	=	'35'
istr_mant.argumento[4]	=	''
istr_mant.argumento[5]	=	""
istr_mant.argumento[7]	=	''

dw_3	=	tab_1.tp_1.dw_detafruta
dw_5	=	tab_1.tp_2.dw_envrec
dw_7	=	tab_1.tp_3.dw_envdes

dw_2.GetChild("clpr_rut", idwc_cliente)
idwc_cliente.SetTransObject(sqlca)

IF idwc_cliente.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Clientes")
	idwc_cliente.InsertRow(0)
END IF

dw_3.GetChild("cama_codigo",idwc_Camara)
idwc_Camara.SetTransObject(SQLCA)
idwc_Camara.Retrieve(gstr_ParamPlanta.CodigoPlanta)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 84")

dw_3.Modify("datawindow.message.title='Error '+ is_titulo")
dw_3.Modify("DataWindow.Footer.Height = 84")

dw_5.Modify("datawindow.message.title='Error '+ is_titulo")
dw_5.Modify("DataWindow.Footer.Height = 84")

dw_7.Modify("datawindow.message.title='Error '+ is_titulo")
dw_7.Modify("DataWindow.Footer.Height = 84")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.solo_consulta			=	False

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

iuo_Especie			=	Create uo_especie
iuo_PesoEstanEspe	=	Create uo_PesoEstanEspe
iuo_FechaMovto		=	Create uo_FechaMovto

iuo_tipomovtofruta	=	Create uo_tipomovtofruta
iuo_TipoMovtoEnva		=	Create uo_tipomovtofruta


end event

event resize;//
end event

event ue_borra_detalle();call super::ue_borra_detalle;Integer	li_tabpage, li_borra
Boolean	lb_estado
str_mant_envases	lstr_mant

li_tabpage	=	tab_1.SelectedTab

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

lb_estado			=	istr_mant.Solo_Consulta

IF li_tabpage <> 1 THEN
	lstr_mant.argumento				=	istr_mant.argumento
	lstr_mant.dw						=	istr_mant.dw
	lstr_mant.dw2						=	istr_mant.dw2
	lstr_mant.Agrega					=	istr_mant.Agrega
	lstr_mant.Borra					=	istr_mant.Borra
	lstr_mant.Solo_Consulta			=	istr_mant.Solo_Consulta
	lstr_mant.respuesta				=	istr_mant.respuesta
	lstr_mant.tipo						=	istr_mant.tipo
	lstr_mant.usuariosoloconsulta	=	istr_mant.usuariosoloconsulta
	
	ProductoresLotes(lstr_mant.productores)
END IF

CHOOSE CASE li_tabpage
	CASE 1 
		
		istr_mant.argumento[9]   = String(dw_2.Object.mfco_tipdoc[1])	
		istr_mant.argumento[10]  = String(dw_2.Object.mfco_docrel[1])
			
		istr_mant.dw	=	dw_3

		OpenWithParm(iw_mantencion_1, istr_mant)
		
	CASE 2
		
		lstr_mant.dw	=	dw_5
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'2'
		OpenWithParm(iw_mantencion_2, lstr_mant)
		
	CASE 3
		lstr_mant.dw	=	dw_7
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'1'
		OpenWithParm(iw_mantencion_2, lstr_mant)
		
END CHOOSE

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	CHOOSE CASE li_tabpage
		CASE 1
			li_borra	=	dw_3.DeleteRow(0)

	CASE 2
			li_borra	=	dw_5.DeleteRow(0)
		CASE 3
			li_borra	=	dw_7.DeleteRow(0)
	END CHOOSE
 
	IF li_borra = 1 THEN
		
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_3.RowCount() = 0 THEN pb_eli_det.Enabled = False
END IF

istr_mant.borra	 = False

end event

event ue_borrar();IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_3.RowCount() > 0 THEN dw_3.RowsMove(1,dw_3.RowCount(),Primary!,dw_3,1,Delete!)
IF dw_7.RowCount() > 0 THEN dw_7.RowsMove(1,dw_7.RowCount(),Primary!,dw_7,1,Delete!)
IF dw_5.RowCount() > 0 THEN dw_5.RowsMove(1,dw_5.RowCount(),Primary!,dw_5,1,Delete!)
IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		
		ib_AutoCommit		=	SQLCA.AutoCommit
		SQLCA.AutoCommit	=	False
		
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_borrar = False
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF
end event

event ue_guardar();IF dw_3.AcceptText() = -1 THEN RETURN
IF dw_7.AcceptText() = -1 THEN RETURN
IF dw_5.AcceptText() = -1 THEN RETURN

CALL SUPER::ue_guardar

IF Message.DoubleParm = -1 THEN RETURN


end event

event ue_modifica_detalle();Integer	li_tabpage
Boolean	lb_estado

str_mant_envases	lstr_mant

li_tabpage	=	tab_1.SelectedTab

istr_mant.agrega	= False
istr_mant.borra	= False

lb_estado			=	istr_mant.Solo_Consulta

IF li_tabpage <> 1 THEN
	lstr_mant.argumento				=	istr_mant.argumento
	lstr_mant.dw						=	istr_mant.dw
	lstr_mant.dw2						=	istr_mant.dw2
	lstr_mant.Agrega					=	istr_mant.Agrega
	lstr_mant.Borra					=	istr_mant.Borra
	lstr_mant.Solo_Consulta			=	istr_mant.Solo_Consulta
	lstr_mant.respuesta				=	istr_mant.respuesta
	lstr_mant.tipo						=	istr_mant.tipo
	lstr_mant.usuariosoloconsulta	=	istr_mant.usuariosoloconsulta
	
END IF

CHOOSE CASE li_tabpage
	CASE 1
		
		IF dw_3.RowCount() > 0 THEN
			
			istr_mant.argumento[9]   = String(dw_2.Object.mfco_tipdoc[1])	
			istr_mant.argumento[10]  = String(dw_2.Object.mfco_docrel[1])
			istr_mant.dw	=	dw_3

			OpenWithParm(iw_mantencion_1, istr_mant)
		END IF
		istr_mant.Solo_Consulta	=	lb_estado

	CASE 2				
		
		IF dw_5.RowCount() > 0 THEN
			lstr_mant.dw	=	dw_5
			SetNull(lstr_mant.dw2)
			lstr_mant.Argumento[4]	=	'2'
			OpenWithParm(iw_mantencion_2, lstr_mant)
		END IF
		lstr_mant.Solo_Consulta	=	lb_estado
		
	CASE 3				
		
		IF dw_7.RowCount() > 0 THEN
			lstr_mant.dw	=	dw_7
			SetNull(lstr_mant.dw2)
			lstr_mant.Argumento[4]	=	'1'
			OpenWithParm(iw_mantencion_2, lstr_mant)
		END IF
END CHOOSE


end event

event ue_nuevo();Long		ll_modif

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif	=	dw_1.GetNextModified(0, Primary!)
			ll_modif	+=	dw_2.GetNextModified(0, Primary!)
			ll_modif	+=	dw_3.GetNextModified(0, Primary!)
			ll_modif	+=	dw_5.GetNextModified(0, Primary!)
			ll_modif	+=	dw_7.GetNextModified(0, Primary!)
		
			IF dw_1.RowCount() > 0 and ll_modif > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.TriggerEvent("ue_guardar")
						IF message.DoubleParm = -1 THEN ib_ok = False
					CASE 3
						ib_ok	= False
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF

IF Not ib_ok THEN RETURN

dw_1.Reset()
dw_3.Reset()
dw_5.Reset()
dw_7.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_2.SetFocus()

dw_2.Object.plde_codigo[1]	=	gstr_ParamPlanta.CodigoPlanta
dw_2.Object.tpmv_codigo[1]	=	integer(istr_mant.argumento[2])
dw_2.Object.mfco_estmov[1]	=	3

idt_FechaSistema	=	F_FechaHora()

dw_2.Object.mfco_fecmov[1]	=	Date(idt_FechaSistema)

istr_Mant.Argumento[3]			=	''
istr_mant.Argumento[7]			=	'0'

tab_1.tp_1.Enabled	=	False
tab_1.tp_2.Enabled	=	False
tab_1.tp_3.Enabled	=	False
pb_ins_det.Enabled	=	False

tab_1.SelectTab(2)
HabilitaEncab(True)		

end event

event ue_nuevo_detalle();Integer	li_tabpage
Boolean	lb_estado

str_mant_envases	lstr_mant

li_tabpage			=	tab_1.SelectedTab

istr_mant.Borra	=	False
istr_mant.Agrega	=	True
	
lb_estado			=	istr_mant.Solo_Consulta

IF li_tabpage <> 1 THEN
	
	lstr_mant.argumento				=	istr_mant.argumento
	lstr_mant.dw						=	istr_mant.dw
	lstr_mant.dw2						=	istr_mant.dw2
	lstr_mant.Agrega					=	istr_mant.Agrega
	lstr_mant.Borra					=	istr_mant.Borra
	lstr_mant.Solo_Consulta			=	istr_mant.Solo_Consulta
	lstr_mant.respuesta				=	istr_mant.respuesta
	lstr_mant.tipo						=	istr_mant.tipo
	lstr_mant.usuariosoloconsulta	=	istr_mant.usuariosoloconsulta
	
END IF

CHOOSE CASE li_tabpage
	CASE 1

		istr_mant.argumento[9]   = String(dw_2.Object.mfco_tipdoc[1])	
		istr_mant.argumento[10]  = String(dw_2.Object.mfco_docrel[1])
		
		istr_mant.dw	=	dw_3

		OpenWithParm(iw_mantencion_1, istr_mant)
		istr_mant.Solo_Consulta	=	lb_estado

	CASE 2
		
		lstr_mant.dw	=	dw_5
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'2'
		OpenWithParm(iw_mantencion_2, lstr_mant)
		lstr_mant.Solo_Consulta	=	lb_estado

	CASE 3
		
		lstr_mant.dw	=	dw_7
		SetNull(lstr_mant.dw2)
		lstr_mant.Argumento[4]	=	'1'
		OpenWithParm(iw_mantencion_2, lstr_mant)
		
END CHOOSE

IF dw_3.RowCount() > 0 AND dw_5.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_3.RowCount() > 0 AND dw_5.RowCount() > 0 AND Not pb_eliminar.Enabled THEN
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
	pb_eli_det.Enabled	=	True
END IF

CHOOSE CASE li_tabpage 
	CASE 1
		istr_mant	=	Message.PowerObjectParm
		
		dw_3.SetRow(il_Fila)
		dw_3.SelectRow(il_Fila, True)
	CASE 2
		
		dw_7.SetRow(il_Fila)
		dw_7.SelectRow(il_Fila, True)
	CASE 3
		
		dw_5.SetRow(il_Fila)
		dw_5.SelectRow(il_Fila, True)
END CHOOSE


end event

event ue_recuperadatos();Long	ll_fila_d, ll_fila_e, respuesta, ll_fila_env

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]),&
										 Integer(istr_mant.argumento[2]),&
										 Long(istr_mant.argumento[3]))

	IF ll_fila_e = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila_e > 0 THEN
		
		tab_1.tp_2.Enabled		=	True
		
		
		HabilitaEncab(False)

		DO
			IF dw_1.Retrieve(Integer(istr_mant.argumento[1]),&
							     Integer(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3])) = -1 OR &
				dw_3.Retrieve(Integer(istr_mant.argumento[1]),&
							     Integer(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3])) = -1 OR &
				dw_5.Retrieve(Integer(istr_mant.argumento[1]),&
							    Integer(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),1) = -1 OR &
				dw_7.Retrieve(Integer(istr_mant.argumento[1]),&
							     Integer(istr_mant.argumento[2]),&
							     Long(istr_mant.argumento[3]),2) = -1  THEN
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)

			ELSE
				
				pb_imprimir.Enabled	= True
				pb_eliminar.Enabled  = NOT istr_mant.Solo_Consulta
				pb_grabar.Enabled		= NOT istr_mant.Solo_Consulta
				pb_ins_det.Enabled	= NOT istr_mant.Solo_Consulta
				pb_eli_det.Enabled	= NOT istr_mant.Solo_Consulta
				dw_3.SetRow(1)
				dw_3.SelectRow(1,True)
				dw_3.SetFocus()
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_seleccion();call super::ue_seleccion;IF ib_ok = False THEN RETURN

Str_Busqueda	lstr_busq

lstr_busq.argum[1] = String(gstr_param.plde_codigo)
lstr_busq.argum[2] = '35'								// Movimiento de Recepción
lstr_busq.argum[3] = '3'									// Cualquier estado
lstr_busq.argum[4] = String(idt_FechaSistema)   // Desde Fecha de Inicio Ducha

OpenWithParm(w_busc_movtofrutacomenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	
	istr_mant.argumento[3]	=	lstr_busq.argum[3]
	
	This.TriggerEvent("ue_recuperadatos")
END IF
end event

event ue_imprimir();//Long	ll_modif
//
//IF Not istr_mant.Solo_Consulta THEN
//	CHOOSE CASE wf_modifica()
//		CASE -1
//			ib_ok = False
//		CASE 0
//			ll_modif	=	dw_1.GetNextModified(0, Primary!)
//			ll_modif	+=	dw_2.GetNextModified(0, Primary!)
//			ll_modif	+=	dw_3.GetNextModified(0, Primary!)
//			ll_modif	+=	dw_5.GetNextModified(0, Primary!)
//			ll_modif	+=	dw_7.GetNextModified(0, Primary!)
//		
//			IF dw_3.RowCount() > 0 and ll_modif > 0 THEN
//				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información antes de Imprimir ?", Question!, YesNoCancel!)
//					CASE 1
//						Message.DoubleParm = 0
//						This.TriggerEvent("ue_guardar")
//						IF message.DoubleParm = -1 THEN ib_ok = False
//					CASE 3
//						ib_ok	= False
//						RETURN
//				END CHOOSE
//			END IF
//	END CHOOSE
//END IF
//
//IF Not ib_ok THEN RETURN
//
//SetPointer(HourGlass!)
//
//Long		fila
//Integer  li_estado, li_Kilos
//
//istr_info.titulo	= "GUIA DE DESPACHO A VENTA FRUTA COMERCIAL"
//istr_info.copias	= 1
//
//OpenWithParm(vinf,istr_info)
//
//li_estado	=	dw_2.object.mfco_estmov[1]
//
//IF li_estado=1 or li_estado=2 THEN
//   vinf.dw_1.DataObject = "dw_info_guia_venta_transitoria"
//ElSE
//	vinf.dw_1.DataObject = "dw_info_guia_venta_Definitiva"
//END IF
//
//vinf.dw_1.SetTransObject(sqlca)
//
//fila = vinf.dw_1.Retrieve(Integer(istr_mant.Argumento[1]),&
//								  Integer(istr_mant.Argumento[2]),&
//								  Integer(istr_mant.Argumento[3]),li_estado)
//
//IF fila = -1 THEN
//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
//ELSEIF fila = 0 THEN
//	MessageBox( "No Existe información", "No existe información para este informe.", &
//					StopSign!, Ok!)
//ELSE
//	F_Membrete(vinf.dw_1)
//	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
//	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
//
//	vinf.Visible	= True
//	vinf.Enabled	= True
//END IF
//
//SetPointer(Arrow!)
end event

event closequery;IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			Message.ReturnValue = 1 
		CASE 0
			IF dw_3.RowCount() > 0 THEN
				CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
					CASE 1
						Message.DoubleParm = 0
						This.triggerevent("ue_guardar")
						IF message.doubleparm = -1 THEN Message.ReturnValue = 1
						RETURN
					CASE 3
						Message.ReturnValue = 1
						RETURN
				END CHOOSE
			END IF
	END CHOOSE
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_traspasoventacliente
boolean visible = false
integer x = 187
integer y = 1404
integer width = 1490
integer height = 308
string title = "Detalle de Movimiento de Envase"
string dataobject = "dw_mant_movtoenvaenca_comercial"
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_traspasoventacliente
integer x = 32
integer y = 56
integer width = 3127
integer height = 532
string dataobject = "dw_mant_movtofruta_traspaso_venta"
boolean livescroll = true
end type

event dw_2::itemchanged;String	ls_Nula,	ls_Columna
Date		ldt_Fecha

ls_Columna = GetColumnName()
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
		
	CASE "mfco_numero"
		
		IF NOT ExisteRecepcion(gstr_ParamPlanta.CodigoPlanta, Integer(istr_mant.argumento[2]), Integer(data)) THEN
			This.SetItem(1,"mfco_numero",Long(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSE
			istr_mant.argumento[3] = Data
		END IF
    
	CASE "mfco_fecmov"
		ldt_Fecha	=	Date(Data)
		
		IF NOT iuo_FechaMovto.Valida_FechaMovto(ldt_Fecha) THEN
			This.SetItem(1,"mfco_fecmov",Date(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF

   CASE "mfco_docrel"
		
		IF Not existeorden(long(data),dw_2.Object.mfco_tipdoc[1]) THEN
			This.SetItem(1, ls_Columna, long(ls_Nula))
			RETURN 1
		END IF

END CHOOSE

HabilitaIngreso(ls_Columna)

end event

event dw_2::buttonclicked;CHOOSE CASE dwo.Name
	
	CASE "b_orden"
	 	buscaOrden()
			
END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_traspasoventacliente
integer x = 3278
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_traspasoventacliente
integer x = 3278
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_traspasoventacliente
integer x = 3278
integer y = 632
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_traspasoventacliente
integer x = 3278
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_traspasoventacliente
integer x = 3278
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_traspasoventacliente
integer x = 3282
integer y = 1464
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_traspasoventacliente
integer y = 1636
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_traspasoventacliente
integer x = 3278
end type

type tab_1 from tab within w_maed_traspasoventacliente
integer x = 27
integer y = 640
integer width = 3118
integer height = 1220
integer taborder = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean fixedwidth = true
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 2
tp_1 tp_1
tp_2 tp_2
tp_3 tp_3
end type

on tab_1.create
this.tp_1=create tp_1
this.tp_2=create tp_2
this.tp_3=create tp_3
this.Control[]={this.tp_1,&
this.tp_2,&
this.tp_3}
end on

on tab_1.destroy
destroy(this.tp_1)
destroy(this.tp_2)
destroy(this.tp_3)
end on

type tp_1 from userobject within tab_1
string tag = "Detalle de Lotes Ingresados"
integer x = 18
integer y = 112
integer width = 3081
integer height = 1092
boolean enabled = false
long backcolor = 12632256
string text = "Detalle de Fruta Salida"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "DatabaseProfile5!"
long picturemaskcolor = 536870912
dw_detafruta dw_detafruta
end type

on tp_1.create
this.dw_detafruta=create dw_detafruta
this.Control[]={this.dw_detafruta}
end on

on tp_1.destroy
destroy(this.dw_detafruta)
end on

type dw_detafruta from uo_dw within tp_1
integer x = 18
integer y = 36
integer width = 3026
integer height = 1024
integer taborder = 10
string dataobject = "dw_mues_movtofrutacomdeta_ventas"
boolean hscrollbar = true
boolean livescroll = true
end type

event doubleclicked;call super::doubleclicked;IF dw_5.RowCount()>0 THEN
   w_maed_movtofrutacomercial.TriggerEvent("ue_modifica_detalle")
END IF


RETURN 0
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutacomercial.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutacomercial.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

type tp_2 from userobject within tab_1
string tag = "Registro de Envases recepcionados con y sin fruta"
integer x = 18
integer y = 112
integer width = 3081
integer height = 1092
boolean enabled = false
long backcolor = 12632256
string text = "Envases Retirados"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "CreateRuntime!"
long picturemaskcolor = 536870912
dw_envrec dw_envrec
end type

on tp_2.create
this.dw_envrec=create dw_envrec
this.Control[]={this.dw_envrec}
end on

on tp_2.destroy
destroy(this.dw_envrec)
end on

type dw_envrec from uo_dw within tp_2
integer x = 27
integer y = 52
integer width = 3026
integer height = 992
integer taborder = 11
string dataobject = "dw_mues_movtoenvadeta_recfruta_comer"
boolean hscrollbar = true
boolean livescroll = true
end type

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutacomercial.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event doubleclicked;call super::doubleclicked;IF dw_5.RowCount()>0 THEN
   w_maed_movtofrutacomercial.TriggerEvent("ue_modifica_detalle")
END IF

RETURN 0
end event

type tp_3 from userobject within tab_1
string tag = "Registro de Envases que retira para cosecha"
integer x = 18
integer y = 112
integer width = 3081
integer height = 1092
boolean enabled = false
long backcolor = 12632256
string text = "Detalle Fruta Entrada"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Inherit!"
long picturemaskcolor = 536870912
dw_envdes dw_envdes
end type

on tp_3.create
this.dw_envdes=create dw_envdes
this.Control[]={this.dw_envdes}
end on

on tp_3.destroy
destroy(this.dw_envdes)
end on

type dw_envdes from uo_dw within tp_3
integer x = 23
integer y = 32
integer width = 3026
integer height = 1040
integer taborder = 21
string dataobject = "dw_mues_movtoenvadeta_recfruta_comer"
boolean hscrollbar = true
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutacomercial.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event doubleclicked;call super::doubleclicked;IF dw_5.RowCount()>0 THEN
   w_maed_movtofrutacomercial.TriggerEvent("ue_modifica_detalle")
END IF


RETURN 0
end event

