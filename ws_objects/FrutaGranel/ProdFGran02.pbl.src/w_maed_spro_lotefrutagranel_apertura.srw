$PBExportHeader$w_maed_spro_lotefrutagranel_apertura.srw
$PBExportComments$Apertura de Lotes Fruta Granel
forward
global type w_maed_spro_lotefrutagranel_apertura from w_mant_encab_deta_csd
end type
type dw_3 from uo_dw within w_maed_spro_lotefrutagranel_apertura
end type
type ids_base from datawindow within w_maed_spro_lotefrutagranel_apertura
end type
type ids_lotedeta from datawindow within w_maed_spro_lotefrutagranel_apertura
end type
type ids_movto from datawindow within w_maed_spro_lotefrutagranel_apertura
end type
type ids_movtoenca from datawindow within w_maed_spro_lotefrutagranel_apertura
end type
end forward

global type w_maed_spro_lotefrutagranel_apertura from w_mant_encab_deta_csd
integer width = 3589
integer height = 2320
string title = "APERTURA DE LOTES"
string menuname = ""
event ue_imprimir ( )
event ue_validaregistro ( )
dw_3 dw_3
ids_base ids_base
ids_lotedeta ids_lotedeta
ids_movto ids_movto
ids_movtoenca ids_movtoenca
end type
global w_maed_spro_lotefrutagranel_apertura w_maed_spro_lotefrutagranel_apertura

type variables
uo_productores			iuo_productores
uo_lotesfrutagranel	iuo_Lotes
uo_condicioncc			iuo_CondiCC

str_variedad			istr_variedad
str_categoria			istr_categoria

DataWindowChild   	idwc_planta, idwc_especie, idwc_variedad

Boolean					ib_AutoCommit
end variables

forward prototypes
public subroutine cambiolote (integer ai_tipoenvase, integer ai_envase, integer ai_cantbultos)
public function boolean existeespecie (string as_valor)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine buscaloteorigen ()
public subroutine habilitabotton (string columna)
end prototypes

event ue_imprimir();SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DE TIPOS DE MOVIMIENTOS / DOCUMENTOS DE MOVIMIENTOS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_tipomovtofruta_doctomovtodeta"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de Datos : ~n" + sqlca.SQLErrText, StopSign!, OK!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No Existe información para este informe.", &
					StopSign!, OK!)
	
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
	
	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
					
end event

event ue_validaregistro();Integer	li_cont
String	ls_mensaje, ls_colu[]
	

	IF Isnull(dw_1.Object.prdu_codigo[il_fila]) OR dw_1.Object.prdu_codigo[il_fila] = "" THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo Producto "
		ls_colu[li_cont]	= "prdu_codigo"
	END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

public subroutine cambiolote (integer ai_tipoenvase, integer ai_envase, integer ai_cantbultos);Long		ll_FilaI, ll_FilaD

ll_FilaI	=	ids_LoteDeta.Find( &
				"lote_pltcod = " + String(dw_2.Object.lote_pltcod[1]) + " AND " + &
				"lote_espcod = " + String(dw_2.Object.lote_espcod[1]) + " AND " + &
				"lote_codigo = " + String(ids_Base.Object.lote_codigo[1]) + " AND " + &
				"enva_tipoen = " + String(ai_TipoEnvase) + " AND " + &
				"enva_codigo = " + String(ai_Envase), 1, ids_LoteDeta.RowCount())

ids_LoteDeta.Object.lotd_totbul[ll_FilaI]	=	ids_LoteDeta.Object.lotd_totbul[ll_FilaI] - &
															ai_CantBultos
ids_LoteDeta.Object.lotd_totnet[ll_FilaI]	=	ids_LoteDeta.Object.lotd_totbul[ll_FilaI] * &
															ids_LoteDeta.Object.lotd_kilpro[ll_FilaI]

//	Actualiza Totales de Encabezado
ids_Base.Object.lote_totbul[ll_FilaI]		=	ids_Base.Object.lote_totbul[ll_FilaI] - &
															ai_CantBultos
ids_Base.Object.lote_totnet[ll_FilaI]		=	ids_Base.Object.lote_totnet[ll_FilaI] - &
															(ai_CantBultos * &
															ids_LoteDeta.Object.lotd_kilpro[ll_FilaI])
IF ids_Base.Object.lote_totbul[ll_FilaI] = 0 THEN
	ids_Base.Object.lote_kilpro[ll_FilaI] = 0
ELSE
	ids_Base.Object.lote_kilpro[ll_FilaI]	=	ids_Base.Object.lote_totnet[ll_FilaI] / &
															ids_Base.Object.lote_totbul[ll_FilaI]
END IF

ll_FilaD												=	ids_LoteDeta.InsertRow(0)
ids_LoteDeta.Object.lote_pltcod[ll_FilaD]	=	ids_LoteDeta.Object.lote_pltcod[ll_FilaI]
ids_LoteDeta.Object.lote_espcod[ll_FilaD]	=	ids_LoteDeta.Object.lote_espcod[ll_FilaI]
ids_LoteDeta.Object.lote_codigo[ll_FilaD]	=	dw_2.Object.lote_codigo[1]
ids_LoteDeta.Object.enva_tipoen[ll_FilaD]	=	ids_LoteDeta.Object.enva_tipoen[ll_FilaI]
ids_LoteDeta.Object.enva_codigo[ll_FilaD]	=	ids_LoteDeta.Object.enva_codigo[ll_FilaI]
ids_LoteDeta.Object.lotd_totbul[ll_FilaD]	=	ai_CantBultos
ids_LoteDeta.Object.lotd_kilpro[ll_FilaD]	=	ids_LoteDeta.Object.lotd_kilpro[ll_FilaI]
ids_LoteDeta.Object.lotd_totnet[ll_FilaD]	=	ai_CantBultos * &
															ids_LoteDeta.Object.lotd_kilpro[ll_FilaI]

//	Actualiza Totales de Encabezado
dw_2.Object.lote_totbul[1]						=	dw_2.Object.lote_totbul[1] + &
															ai_CantBultos
dw_2.Object.lote_totnet[1]						=	dw_2.Object.lote_totnet[1] + &
															ids_LoteDeta.Object.lotd_totnet[ll_FilaD]
IF dw_2.Object.lote_totbul[1] = 0 THEN
	dw_2.Object.lote_kilpro[1]	=	0
ELSE
	dw_2.Object.lote_kilpro[1]					=	dw_2.Object.lote_totnet[1] / &
															dw_2.Object.lote_totbul[1]
END IF

RETURN
end subroutine

public function boolean existeespecie (string as_valor);Integer	espe_codigo
String	nombre

espe_codigo	= Integer(as_valor)
		

IF IsNull(espe_codigo) = False THEN
	istr_mant.argumento[2]	= String(espe_codigo)
	
	 SELECT espe_nombre
    INTO :nombre 
    FROM  dba.especies 
   WHERE  espe_codigo = :espe_codigo;

	IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Especies" )
	ELSEIF sqlca.SQLCode = 0 THEN
		RETURN TRUE
	ELSE
//		dw_especies.setitem(1, "espe_codigo", 1)
	END IF
END IF
RETURN FALSE

end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_Retorno
Integer	li_Valor

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

IF Not Borrando THEN
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF dw_3.Update(True, False) = 1 THEN
				IF ids_Base.Update(True, False) = 1 THEN
					IF ids_LoteDeta.Update(True, False) = 1 THEN
						IF ids_Movtoenca.Update(True, False) = 1 THEN
							IF ids_Movto.Update(True, False) = 1 THEN
								Commit;
							
								IF sqlca.SQLCode <> 0 THEN
									F_ErrorBaseDatos(sqlca, This.Title)
								ELSE
									lb_Retorno	=	True
								
									dw_1.ResetUpdate()
									dw_2.ResetUpdate()
									dw_3.ResetUpdate()
									ids_Base.ResetUpdate()
									ids_Movtoenca.ResetUpdate()
									ids_Movto.ResetUpdate()
									ids_LoteDeta.ResetUpdate()
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

public subroutine buscaloteorigen ();str_busqueda	lstr_busq

lstr_busq.Argum[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
lstr_busq.Argum[2]	=	istr_mant.Argumento[2]

OpenWithParm(w_busc_spro_lotefrutagranel, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.Argum[3] = "" THEN
	dw_2.SetColumn("lote_codigo")
	dw_2.SetFocus()
ELSE
	istr_mant.Argumento[1]	=	lstr_busq.Argum[1]
	istr_mant.Argumento[2]	=	lstr_busq.Argum[2]
	istr_mant.Argumento[3]	=	lstr_busq.Argum[3]
	
	dw_3.Title	=	'Detalle de Lote Origen : ' + &
						String(Integer(lstr_busq.Argum[1]), '0000') + &
						String(Integer(lstr_busq.Argum[2]), '00') + &
						String(Integer(lstr_busq.Argum[3]), '0000')
	
	This.TriggerEvent("ue_recuperadatos")
END IF

RETURN
end subroutine

public subroutine habilitabotton (string columna);Boolean	lb_Estado = True
Date		ld_Fecha
Time		lt_Hora
	
IF IsNull(dw_2.Object.duch_codigo[il_fila]) OR dw_2.Object.duch_codigo[il_fila] = 0 THEN
	lb_Estado	=	False
END IF

IF IsNull(dw_2.Object.codu_nropos[il_fila]) OR dw_2.Object.codu_nropos[il_fila] = 0 THEN
	lb_Estado	=	False
END IF

IF IsNull(dw_2.Object.codu_fecini[il_fila]) OR dw_2.Object.codu_fecini[il_fila] = ld_Fecha THEN
	lb_Estado	=	False
END IF

IF IsNull(dw_2.Object.codu_horini[il_fila]) OR dw_2.Object.codu_horini[il_fila] = lt_Hora THEN
	lb_Estado	=	False
END IF

IF IsNull(dw_2.Object.bpdu_feceve[il_fila]) OR dw_2.Object.bpdu_feceve[il_fila] = ld_Fecha THEN
	lb_Estado	=	False
END IF

pb_ins_det.Enabled	=	lb_Estado
end subroutine

on w_maed_spro_lotefrutagranel_apertura.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.ids_base=create ids_base
this.ids_lotedeta=create ids_lotedeta
this.ids_movto=create ids_movto
this.ids_movtoenca=create ids_movtoenca
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.ids_base
this.Control[iCurrent+3]=this.ids_lotedeta
this.Control[iCurrent+4]=this.ids_movto
this.Control[iCurrent+5]=this.ids_movtoenca
end on

on w_maed_spro_lotefrutagranel_apertura.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.ids_base)
destroy(this.ids_lotedeta)
destroy(this.ids_movto)
destroy(this.ids_movtoenca)
end on

event open;DataWindowChild   ldwc_variedad, ldwc_TipoFrio, ldwc_PeriodoFrio, &
						ldwc_Condicion, ldwc_Categoria, ldwc_Planta, ldwc_TipoEnvase

iuo_productores	=	Create uo_productores
iuo_CondiCC			=	Create uo_condicioncc
iuo_Lotes			=	Create uo_lotesfrutagranel

dw_2.GetChild("lote_pltcod", idwc_planta)
idwc_planta.SetTransObject(sqlca)

IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

dw_2.GetChild("lote_espcod", idwc_especie)
idwc_especie.SetTransObject(sqlca)

IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Especies")
	idwc_especie.InsertRow(0)
END IF

dw_2.GetChild("vari_codigo", idwc_variedad)
idwc_variedad.SetTransObject(sqlca)

IF idwc_variedad.Retrieve(gstr_ParamPlanta.CodigoEspecie) = 0 THEN
	MessageBox("Atención","Falta Registrar Variedades")
	idwc_variedad.InsertRow(0)
ELSE
	ids_Base.GetChild("vari_codigo", ldwc_variedad)
	ldwc_variedad.SetTransObject(sqlca)
	
	IF ldwc_variedad.Retrieve(gstr_ParamPlanta.CodigoEspecie) = 0 THEN
		ldwc_variedad.InsertRow(0)
	END IF
END IF

dw_2.GetChild("frio_tipofr", ldwc_TipoFrio)
ldwc_TipoFrio.SetTransObject(sqlca)

IF ldwc_TipoFrio.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Tipo de Frio")
	ldwc_TipoFrio.InsertRow(0)
END IF

dw_2.GetChild("pefr_codigo", ldwc_PeriodoFrio)
ldwc_PeriodoFrio.SetTransObject(sqlca)

IF ldwc_PeriodoFrio.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Periodos de Frío")
	ldwc_PeriodoFrio.InsertRow(0)
END IF

dw_2.GetChild("cocc_codigo", ldwc_Condicion)
ldwc_Condicion.SetTransObject(sqlca)

IF ldwc_Condicion.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Condición")
	ldwc_Condicion.InsertRow(0)
END IF

dw_2.GetChild("cate_codigo", ldwc_Categoria)
ldwc_Categoria.SetTransObject(sqlca)

IF ldwc_Categoria.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Condición")
	ldwc_Categoria.InsertRow(0)
END IF

dw_3.GetChild("plde_codigo", ldwc_Planta)
ldwc_Planta.SetTransObject(sqlca)

IF ldwc_Planta.Retrieve() = 0 THEN
	idwc_planta.InsertRow(0)
END IF

dw_3.GetChild("enva_tipoen", ldwc_TipoEnvase)
ldwc_TipoEnvase.SetTransObject(sqlca)

IF ldwc_TipoEnvase.Retrieve() = 0 THEN
	MessageBox("Atención","Falta Registrar Tipos de Envase")
	ldwc_TipoEnvase.InsertRow(0)
END IF

Call Super::Open

dw_3.SetTransObject(sqlca)
dw_3.Modify("datawindow.message.title='Error '+ is_titulo")
dw_3.Modify("DataWindow.Footer.Height = 110")

buscar  = ""
ordenar = ""

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	String(gstr_ParamPlanta.CodigoEspecie)
istr_Mant.Argumento[3]	=	''
istr_Mant.Argumento[4]	=	''

ids_Base.SetTransObject(sqlca)
ids_LoteDeta.SetTransObject(sqlca)
ids_Movto.SetTransObject(sqlca)
ids_MovtoEnca.SetTransObject(sqlca)
end event

event ue_recuperadatos();call super::ue_recuperadatos;Long		ll_fila_d, ll_fila_e, ll_Fila
Integer	li_Respuesta, li_Nulo

SetNull(li_Nulo)

DO
	dw_2.SetRedraw(False)
	
	dw_2.Reset()

	ll_fila_e	= ids_Base.Retrieve(Integer(istr_mant.argumento[1]), &
										 Integer(istr_mant.argumento[2]), &
										 Integer(istr_mant.argumento[3]))

	IF ll_fila_e = -1 THEN
		li_Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			IF dw_3.Retrieve(Integer(istr_mant.argumento[1]), &
								  Integer(istr_mant.argumento[2]), &
								  Integer(istr_mant.argumento[3]), 0) = -1 OR &
				ids_LoteDeta.Retrieve(Integer(istr_mant.argumento[1]), &
								  Integer(istr_mant.argumento[2]), &
								  Integer(istr_mant.argumento[3])) = -1 THEN
				li_Respuesta = MessageBox(	"Error en Base de Datos", &
								"No es posible conectar la Base de Datos.", &
								Information!, RetryCancel!)
			ELSE
				dw_1.Retrieve(0, 0, 0, 1)
				ids_Base.RowsCopy(1, 1, Primary!, dw_2, 1, Primary!)
				dw_3.RowsCopy(1, dw_3.RowCount(), Primary!, dw_1, 1, Primary!)
				
				dw_2.Object.lote_espcod.Protect 				=	1
				dw_2.Object.lote_espcod.BackGround.Color	=	RGB(192,192,192)
				dw_2.Object.lote_codigo.Protect 				=	1
				dw_2.Object.lote_codigo.BackGround.Color	=	RGB(192,192,192)
				dw_2.Object.lote_codigo[1]	=	li_Nulo
				dw_2.Object.lote_totbul[1]	=	0
				dw_2.Object.lote_totnet[1]	=	0
				
				FOR ll_Fila = 1 TO dw_1.RowCount()
					dw_1.Object.lote_codigo[ll_Fila]	=	li_Nulo
					dw_1.Object.caex_canbul[ll_Fila]	=	0
				NEXT
				
				pb_grabar.Enabled	=	True
				
				dw_1.SetRow(1)
				dw_1.SetFocus()
			END IF		
		LOOP WHILE li_Respuesta = 1

		IF li_Respuesta = 2 THEN Close(This)
	END IF
	
	dw_2.SetRedraw(True)
LOOP WHILE li_Respuesta = 1

IF li_Respuesta = 2 THEN Close(This)
end event

event ue_borra_detalle();call super::ue_borra_detalle;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
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
	END IF
END IF
end event

event ue_nuevo_detalle();
istr_mant.borra 	= False
istr_mant.agrega	= True
	
IF il_fila > 0 THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled		= True
END IF

il_fila = dw_1.InsertRow(0)

dw_1.ScrollToRow(il_fila)
dw_1.SetRow(il_fila)
dw_1.SetFocus()
end event

event ue_modifica_detalle();call super::ue_modifica_detalle;//IF dw_1.RowCount() > 0 THEN
//	istr_mant.agrega	= False
//	istr_mant.borra	= False
//
//	OpenWithParm(iw_mantencion, istr_mant)
//END IF
end event

event ue_antesguardar();Long		ll_Fila = 1, ll_FilaM, ll_Numero
Integer	li_Planta, li_Especie, li_NumeroLote, li_LoteOrigen, &
			li_TipoEnvase, li_Envase, li_CantBultos, li_Bultos, li_tipmov

ib_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

li_Planta			=	dw_2.Object.lote_pltcod[1]
li_Especie			=	dw_2.Object.lote_espcod[1]
li_LoteOrigen		=	ids_Base.Object.lote_codigo[1]
li_tipmov 			= 81

//	Bloquea Tabla de Lotes para determinar Número de Lote Nuevo
UPDATE	dba.spro_lotesfrutagranel
	SET	lote_codigo = 0
	WHERE	1 = 2 ;
	
SELECT	IsNull(Max(lote_codigo), 0) + 1
	INTO	:li_NumeroLote
	FROM	dba.spro_lotesfrutagranel
	WHERE	lote_pltcod	=	:li_Planta
	AND	lote_espcod	=	:li_Especie ;

//	Pregunta para Asegurarse del cambio de Lotes
IF MessageBox("Apertura de Lote", "Está seguro de Abrir Lote : " + &
	String(ids_Base.Object.lote_pltcod[1], "0000") + &
	String(ids_Base.Object.lote_espcod[1], "00") + &
	String(ids_Base.Object.lote_codigo[1], "0000") + "~ren Lote : " + &
	String(dw_2.Object.lote_pltcod[1], "0000") + &
	String(dw_2.Object.lote_espcod[1], "00") + &
	String(li_NumeroLote, "0000"), Question!, YesNo!, 2) = 1 THEN
	
	dw_2.Object.lote_codigo[1]	=	li_NumeroLote
	li_TipoEnvase					=	0
	li_Envase						=	0
	
	dw_1.SetSort("lote_pltcod A lote_espcod A lote_codigo A enva_tipoen A " + &
					"enva_codigo A plde_codigo A cama_codigo A caex_nroban A " + &
					"caex_nropos A caex_nropis A" )

	dw_1.Sort()
	
	DO WHILE ll_Fila <= dw_1.RowCount()
		IF dw_1.Object.caex_canbul[ll_Fila] = 0 THEN
			dw_1.DeleteRow(ll_Fila)
		ELSE
			//	Controla Cambio de Lote
			IF dw_1.Object.enva_tipoen[ll_Fila]	<> li_TipoEnvase OR &
				dw_1.Object.enva_codigo[ll_Fila]	<> li_Envase THEN
				
				IF li_TipoEnvase <> 0 AND li_Envase <> 0 THEN
					CambioLote(li_TipoEnvase, li_Envase, li_CantBultos)
				END IF
				
				li_TipoEnvase	=	dw_1.Object.enva_tipoen[ll_Fila]
				li_Envase		=	dw_1.Object.enva_codigo[ll_Fila]
				li_CantBultos	=	0
			END IF

			li_Bultos								=	dw_1.Object.caex_canbul[ll_Fila]
			li_CantBultos							=	li_CantBultos + &
															dw_1.Object.caex_canbul[ll_Fila]
			dw_1.Object.lote_codigo[ll_Fila]	=	dw_2.Object.lote_codigo[1]
			
			IF ids_movto.RowCount()=0 THEN 

				UPDATE	dba.spro_movtocamarafgenca
					SET	mvce_numero = 0
					WHERE	1 = 2;
				
				SELECT	IsNull(Max(mvce_numero), 0) + 1
					INTO	:ll_Numero
					FROM	dba.spro_movtocamarafgenca
				  WHERE	plde_codigo	=	:li_Planta
					AND	tpmv_codigo	=	:li_tipmov;
					
				ll_filaM = ids_movtoenca.InsertRow(0)
				ids_movtoenca.Object.plde_codigo[ll_filaM] = li_planta
				ids_movtoenca.Object.tpmv_codigo[ll_filaM] = li_tipmov
				ids_movtoenca.Object.mvce_numero[ll_filaM] = ll_numero
				ids_movtoenca.Object.mvce_fecmov[ll_filaM] = F_FechaHora()
				ids_movtoenca.Object.cama_codori[ll_filaM] = dw_1.Object.cama_codigo[ll_Fila]
				ids_movtoenca.Object.cama_coddes[ll_filaM] = dw_1.Object.cama_codigo[ll_Fila]
				
			ELSE
				
				ll_numero = ids_movtoenca.Object.mvce_numero[1]
				
			END IF
			//	Por cada Registro con Bultos Genera un Movimiento de Entrada
			ll_FilaM	=	ids_Movto.InsertRow(0)
			ids_movto.Object.plde_codigo[ll_filaM] = 	li_planta
			ids_movto.Object.tpmv_codigo[ll_filaM] = 	li_tipmov
			ids_movto.Object.mvce_numero[ll_filaM] = 	ll_numero
			ids_Movto.Object.lote_pltcod[ll_FilaM]	=	dw_1.Object.lote_pltcod[ll_Fila]
			ids_Movto.Object.lote_espcod[ll_FilaM]	=	dw_1.Object.lote_espcod[ll_Fila]
			ids_Movto.Object.lote_codigo[ll_FilaM]	=	dw_1.Object.lote_codigo[ll_Fila]
			ids_Movto.Object.enva_tipoen[ll_FilaM]	=	dw_1.Object.enva_tipoen[ll_Fila]
			ids_Movto.Object.enva_codigo[ll_FilaM]	=	dw_1.Object.enva_codigo[ll_Fila]
			ids_Movto.Object.mvca_tipomv[ll_FilaM]	=	1
//			ids_Movto.Object.plde_codigo[ll_FilaM]	=	dw_1.Object.plde_codigo[ll_Fila]
			ids_Movto.Object.cama_codigo[ll_FilaM]	=	dw_1.Object.cama_codigo[ll_Fila]
			ids_Movto.Object.mvca_nroban[ll_FilaM]	=	dw_1.Object.caex_nroban[ll_Fila]
			ids_Movto.Object.mvca_nropos[ll_FilaM]	=	dw_1.Object.caex_nropos[ll_Fila]
			ids_Movto.Object.mvca_nropis[ll_FilaM]	=	dw_1.Object.caex_nropis[ll_Fila]
			ids_Movto.Object.mvca_canbul[ll_FilaM]	=	dw_1.Object.caex_canbul[ll_Fila]
			
			//	Por cada Registro con Bultos Genera un Movimiento de Salida
			ll_FilaM		=	ids_Movto.InsertRow(0)
			ids_movto.Object.plde_codigo[ll_filaM] = 	li_planta
			ids_movto.Object.tpmv_codigo[ll_filaM] = 	li_tipmov
			ids_movto.Object.mvce_numero[ll_filaM] = 	ll_numero
			ids_Movto.Object.lote_pltcod[ll_FilaM]	=	dw_1.Object.lote_pltcod[ll_Fila]
			ids_Movto.Object.lote_espcod[ll_FilaM]	=	dw_1.Object.lote_espcod[ll_Fila]
			ids_Movto.Object.lote_codigo[ll_FilaM]	=	li_NumeroLote
			ids_Movto.Object.enva_tipoen[ll_FilaM]	=	dw_1.Object.enva_tipoen[ll_Fila]
			ids_Movto.Object.enva_codigo[ll_FilaM]	=	dw_1.Object.enva_codigo[ll_Fila]
			ids_Movto.Object.mvca_tipomv[ll_FilaM]	=	2
//			ids_Movto.Object.plde_codigo[ll_FilaM]	=	dw_1.Object.plde_codigo[ll_Fila]
			ids_Movto.Object.cama_codigo[ll_FilaM]	=	dw_1.Object.cama_codigo[ll_Fila]
			ids_Movto.Object.mvca_nroban[ll_FilaM]	=	dw_1.Object.caex_nroban[ll_Fila]
			ids_Movto.Object.mvca_nropos[ll_FilaM]	=	dw_1.Object.caex_nropos[ll_Fila]
			ids_Movto.Object.mvca_nropis[ll_FilaM]	=	dw_1.Object.caex_nropis[ll_Fila]
			ids_Movto.Object.mvca_canbul[ll_FilaM]	=	dw_1.Object.caex_canbul[ll_Fila]
		END IF
	
		ll_Fila ++
	LOOP
	
	IF li_TipoEnvase <> 0 AND li_Envase <> 0 THEN
		CambioLote(li_TipoEnvase, li_Envase, li_CantBultos)
	END IF
ELSE
	RollBack;
	
	sqlca.AutoCommit		=	ib_AutoCommit
	Message.DoubleParm	=	-1
END IF
end event

event resize;call super::resize;Integer	li_Maximo

li_Maximo	=	dw_1.width

IF dw_2.width > li_Maximo THEN li_Maximo = dw_2.width

dw_3.x		=	37 + Round((li_Maximo - dw_1.width) / 2, 0)
dw_3.y		=	64 + dw_2.Height
dw_3.Height	=	(This.WorkSpaceHeight() - dw_1.y - 41) / 2

dw_1.x		=	37 + Round((li_Maximo - dw_1.width) / 2, 0)
dw_1.y		=	dw_3.y + dw_3.Height + 4
dw_1.Height	=	dw_3.Height
end event

event ue_nuevo();call super::ue_nuevo;dw_2.SetItem(1, "lote_pltcod", gstr_ParamPlanta.CodigoPlanta)
dw_2.SetItem(1, "lote_espcod", gstr_ParamPlanta.CodigoEspecie)

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_Mant.Argumento[2]	=	String(gstr_ParamPlanta.CodigoEspecie)

dw_2.Object.lote_espcod.Protect 				=	0
dw_2.Object.lote_espcod.BackGround.Color	=	RGB(255,255,255)
dw_2.Object.lote_codigo.Protect 				=	0
dw_2.Object.lote_codigo.BackGround.Color	=	RGB(255,255,255)

dw_3.Reset()

end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_spro_lotefrutagranel_apertura
integer x = 37
integer y = 1336
integer width = 3095
integer height = 792
integer taborder = 20
string title = "Detalle de Lote Destino"
string dataobject = "dw_mues_spro_camaraexistefg_aper"
end type

event dw_1::clicked;//
end event

event dw_1::doubleclicked;//
end event

event dw_1::rowfocuschanged;//
end event

event dw_1::getfocus;//
end event

event type long dw_1::ue_seteafila(unsignedlong wparam, long lparam);il_fila	= This.GetRow()

RETURN 0
end event

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna, ls_Nula
Integer	li_Original

SetNull(ls_Nula)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna
	CASE "caex_canbul"
		li_Original	=	This.Object.caex_canbul[il_Fila]
		
		IF Integer(Data) > dw_3.Object.caex_canbul[il_Fila] + li_Original THEN
			MessageBox("Atención", "No puede traspasar más bultos que los~r" + &
						"originales del Lote.~r~rIngrese otra Cantidad.")
						
			This.SetItem(il_Fila, ls_Columna, li_Original)
			
			RETURN 1
		ELSE
			dw_3.Object.caex_canbul[il_Fila]	=	dw_3.Object.caex_canbul[il_Fila] + &
															li_Original - Integer(Data)
		END IF
		
END CHOOSE
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_spro_lotefrutagranel_apertura
integer x = 37
integer y = 32
integer width = 3077
integer height = 492
integer taborder = 10
string dataobject = "dw_mant_spro_lotesfrutagranel_apertura"
end type

event dw_2::clicked;call super::clicked;//
end event

event dw_2::doubleclicked;call super::doubleclicked;//
end event

event dw_2::itemchanged;String	ls_Null
String	ls_columna

ls_columna = GetColumnName()
SetNull(ls_Null)

CHOOSE CASE ls_columna

   CASE "espe_codigo"
			IF Existeespecie(data) THEN 
				istr_mant.argumento[2] = data
			ELSE	
				MessageBox(	"Error ", "No Existe Especie, Ingrese otra.", &
											Information!, OK!)
				RETURN 1
			END IF

	CASE "lote_codigo"
		IF iuo_Lotes.Existe(gstr_ParamPlanta.CodigoPlanta,&
									Integer(istr_mant.argumento[2]),&
									Integer(data), True, Sqlca) THEN
			istr_mant.argumento[3]	=	data
			Parent.TriggerEvent("ue_recuperadatos")
		ELSE
			This.SetItem(il_fila, "lote_codigo", Integer(ls_Null))

			RETURN 1
		END IF

	CASE "cocc_codigo"
		IF NOT iuo_CondiCC.Existe(Integer(data), True, Sqlca) THEN
			This.SetItem(il_fila, "cocc_codigo", Integer(ls_Null))
			RETURN 1
		END IF

	CASE "cate_codigo"
		IF Not ExisteCategoria(Integer(data), istr_categoria) THEN
			This.SetItem(il_fila, "cate_codigo", Integer(ls_Null))
			RETURN 1
		END IF
		
END CHOOSE

end event

event dw_2::buttonclicked;CHOOSE CASE dwo.Name
	CASE "buscaloteorigen"
		BuscaLoteOrigen()

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_spro_lotefrutagranel_apertura
integer x = 3250
integer y = 324
integer taborder = 40
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_spro_lotefrutagranel_apertura
boolean visible = false
integer x = 3250
integer y = 504
integer taborder = 50
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_spro_lotefrutagranel_apertura
integer x = 3250
integer y = 680
integer taborder = 60
end type

event pb_grabar::clicked;call super::clicked;IF Message.DoubleParm = -1 THEN RETURN

str_mant	lstr_Mant

lstr_Mant.Argumento[1]	=	'1'
lstr_Mant.Argumento[2]	=	String(gstr_paramPlanta.CodigoPlanta)
lstr_mant.argumento[3]	=	String(Integer(istr_mant.argumento[1]),'0000')+&
									String(Integer(istr_mant.argumento[2]),'00')+&
									String(istr_mant.argumento[3],'0000')
lstr_mant.argumento[4]	=	'M'
lstr_Mant.argumento[5]	=	istr_mant.argumento[1]
lstr_mant.argumento[6]	=	istr_mant.argumento[2]
lstr_mant.argumento[7]	=	istr_mant.argumento[3]

OpenWithParm(w_mant_bitacorasitua, lstr_Mant)
end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_spro_lotefrutagranel_apertura
boolean visible = false
integer x = 3250
integer y = 860
integer taborder = 70
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_spro_lotefrutagranel_apertura
integer x = 3250
integer y = 1044
integer taborder = 100
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_spro_lotefrutagranel_apertura
boolean visible = false
integer x = 3264
integer y = 1336
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_spro_lotefrutagranel_apertura
boolean visible = false
integer x = 3264
integer y = 1512
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_spro_lotefrutagranel_apertura
boolean visible = false
integer x = 3255
integer y = 140
integer taborder = 30
boolean enabled = false
end type

type dw_3 from uo_dw within w_maed_spro_lotefrutagranel_apertura
integer x = 37
integer y = 544
integer width = 3095
integer height = 792
integer taborder = 0
boolean bringtotop = true
boolean enabled = false
boolean titlebar = true
string title = "Detalle de Lote Origen : "
string dataobject = "dw_mues_spro_camaraexistefg_aper"
borderstyle borderstyle = stylelowered!
end type

event clicked;//
end event

type ids_base from datawindow within w_maed_spro_lotefrutagranel_apertura
boolean visible = false
integer x = 69
integer y = 2128
integer width = 411
integer height = 432
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_spro_lotesfrutagranel_apertura"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type ids_lotedeta from datawindow within w_maed_spro_lotefrutagranel_apertura
boolean visible = false
integer x = 805
integer y = 2140
integer width = 411
integer height = 432
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_recepcion_lotedeta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type ids_movto from datawindow within w_maed_spro_lotefrutagranel_apertura
boolean visible = false
integer x = 1335
integer y = 2140
integer width = 928
integer height = 432
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_mues_spro_movtocamarafg"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type ids_movtoenca from datawindow within w_maed_spro_lotefrutagranel_apertura
boolean visible = false
integer x = 2281
integer y = 2140
integer width = 782
integer height = 432
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_mues_spro_movtocamarafgenca"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

