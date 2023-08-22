$PBExportHeader$w_maed_movtofrutacomer_granel.srw
$PBExportComments$Ventana de Recepción Fruta Embalada de Proceso
forward
global type w_maed_movtofrutacomer_granel from w_mant_encab_deta_csd
end type
type dw_3 from uo_dw within w_maed_movtofrutacomer_granel
end type
type tab_1 from tab within w_maed_movtofrutacomer_granel
end type
type tp_1 from userobject within tab_1
end type
type dw_9 from datawindow within tp_1
end type
type dw_detalle from uo_dw within tp_1
end type
type tp_1 from userobject within tab_1
dw_9 dw_9
dw_detalle dw_detalle
end type
type tp_2 from userobject within tab_1
end type
type dw_envases from uo_dw within tp_2
end type
type tp_2 from userobject within tab_1
dw_envases dw_envases
end type
type tab_1 from tab within w_maed_movtofrutacomer_granel
tp_1 tp_1
tp_2 tp_2
end type
type em_kilos from editmask within w_maed_movtofrutacomer_granel
end type
type ole_puerta from olecustomcontrol within w_maed_movtofrutacomer_granel
end type
type str_pesaje from structure within w_maed_movtofrutacomer_granel
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

global type w_maed_movtofrutacomer_granel from w_mant_encab_deta_csd
integer width = 3611
integer height = 2052
string title = "INGRESO COMERCIAL DE PROCESO"
string menuname = ""
boolean clientedge = true
event ue_validaregistro ( )
dw_3 dw_3
tab_1 tab_1
em_kilos em_kilos
ole_puerta ole_puerta
end type
global w_maed_movtofrutacomer_granel w_maed_movtofrutacomer_granel

type variables
w_mant_deta_movtoenvadeta			iw_mantencion_2

DataWindowChild		idwc_planta, idwc_tipofrio, idwc_periodofrio, idwc_calidad, &
							idwc_especie, idwc_variedad, idwc_tipoenvase, idwc_envase, &
							idwc_categoria, idwc_camara, idwc_productor, idwc_cliente

DataWindow				dw_4, dw_5
DataStore				dw_6, dw_7

str_envase				istr_envase

uo_variedades			iuo_variedades
uo_categorias			iuo_categorias
uo_productores			iuo_productores
uo_productores			iuo_prodempresa
uo_spro_ordenproceso	iuo_spro_ordenproceso
uo_plantadesp			iuo_planta
uo_tipomovtofruta		iuo_TipoMovtoFruta
uo_tipomovtofruta		iuo_TipoMovtoEnva
uo_clientesprod		iuo_cliente
uo_bins					iuo_bins
uo_calicosechero		iuo_calidad

str_puertacomm			istr_puertacomm

Long     				il_NumFruta = 0, il_NumEnva = 0, il_bins[], il_tarjas[]
Integer              ii_tipool=0
String					is_ultimacol
Boolean					ib_Modifica, ib_AutoCommit

Private:
str_pesaje					wstr_pesaje
end variables

forward prototypes
public subroutine captura_totales ()
public function boolean existecalibres ()
public subroutine buscaenvase ()
public function decimal buscatotalenvases (integer ai_planta, integer ai_tipmov, long al_numero, integer ai_loteplanta, integer ai_loteespe, long al_lote, integer ai_tipoenva, integer ai_envase)
public subroutine RecuperaEncab (integer ai_planta, integer ai_tipmovto, integer al_nummovto, integer ai_especie, integer ai_lote)
public function boolean rescatamovto (integer ai_planta, integer ai_tipdoc, long al_proceso)
public subroutine buscaproductor ()
public function boolean revisaenvases ()
public function boolean existeembalaje (integer ai_tipoenvase, integer ai_envase, string ls_columna)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean loteconmov (long al_lote)
public function boolean existemovimiento (long al_numero)
public function integer tiporecepcion (string as_recepcion, date adt_fechamovto)
public function boolean buscamovto (integer ai_tipdoc, long al_docrel)
public subroutine habilitaingreso (string as_columna)
public function boolean existelote (long al_lote)
public function long nuevofoliolote ()
public subroutine habilitalote ()
public function long nuevolote (long al_revlote)
public subroutine habilitaencab (boolean habilita)
public function boolean borradetallemovto (integer ai_planta, integer ai_especie, long al_lote, integer ai_secuen)
public subroutine buscaordenproceso ()
public function boolean retiraprod (long al_productor, integer ai_especie, integer ai_variedad, integer ai_categoria, date ad_fechalot)
public subroutine asignaclientea_dws ()
public function boolean existetarja (integer cliente, integer planta, long tarja)
public function decimal obtienekilosant (long al_tarja)
public function boolean destarafila (long al_fila)
public subroutine buscacalidad ()
end prototypes

event ue_validaregistro();Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_4.GetItemString(il_fila, "refe_gcalib")) OR dw_4.GetItemString(il_fila, "refe_gcalib") = "" THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nGrupo Calibre"
	ls_colu[li_cont]	= "refe_gcalib"
END IF

IF Isnull(dw_4.GetItemNumber(il_fila, "lfcd_bultos")) OR dw_4.GetItemNumber(il_fila, "lfcd_bultos") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCantidad de Bultos"
	ls_colu[li_cont]	= "lfcd_bultos"
END IF

IF Isnull(dw_4.GetItemNumber(il_fila, "lfcd_kilnet")) OR dw_4.GetItemNumber(il_fila, "lfcd_kilnet") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nKilos Netos"
	ls_colu[li_cont]	= "lfcd_kilnet"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_4.SetColumn(ls_colu[1])
	dw_4.SetFocus()
	Message.DoubleParm = -1
END IF

end event

public subroutine captura_totales ();Long	ll_Fila, ll_Total_Cajas

dw_4.AcceptText()

ll_Fila	=	dw_4.RowCount()

IF  ll_Fila > 0 THEN
	ll_Total_Cajas	=	dw_4.Object.total_cajas[ll_Fila]
END IF

dw_3.Object.paen_ccajas[1]	=	ll_Total_Cajas

RETURN
end subroutine

public function boolean existecalibres ();Integer	li_Cantidad, li_Especie, li_TipoEnvase, li_Envase

li_Especie		=	dw_2.Object.espe_codigo[1]
li_TipoEnvase	=	dw_3.Object.enva_tipoen[1]
li_Envase		=	dw_3.Object.enva_codigo[1]

SELECT	Count(caen_calibr)
	INTO	:li_Cantidad
	FROM	dba.calibresenvase
	WHERE	espe_codigo	=	:li_Especie
	AND	enva_tipoen	=	:li_TipoEnvase
	AND	enva_codigo	=	:li_Envase ;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla CalibresEnvase")

	RETURN False
ELSEIF li_Cantidad = 0 THEN

	RETURN False
END IF

RETURN True
end function

public subroutine buscaenvase ();str_busqueda	lstr_busq
String			ls_Nula

SetNull(ls_Nula)

lstr_busq.argum[1]	=	String(dw_4.Object.enva_tipoen[il_Fila])

OpenWithParm(w_busc_envases, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	dw_4.SetColumn("enva_codigo")
	dw_4.Object.enva_codigo[il_Fila]	=	Integer(ls_Nula)
	dw_4.Object.enva_nombre[il_Fila]	=	ls_Nula
	dw_4.SetFocus()
ELSE
	dw_4.Object.enva_codigo[il_Fila]	=	Integer(lstr_busq.argum[2])
	dw_4.Object.enva_nombre[il_Fila]	=	lstr_busq.argum[3]
	
	ExisteEnvase(dw_4.object.enva_tipoen[il_fila], &
 					 dw_4.object.enva_codigo[il_fila], istr_Envase)
END IF

RETURN
end subroutine

public function decimal buscatotalenvases (integer ai_planta, integer ai_tipmov, long al_numero, integer ai_loteplanta, integer ai_loteespe, long al_lote, integer ai_tipoenva, integer ai_envase);Decimal{2} ld_Bultos

  SELECT sum(lfd.lfcd_bultos) into :ld_Bultos
    FROM dba.spro_movtofrutacomenca mfe, dba.spro_movtofrutacomdeta mfd,
	      dba.spro_lotesfrutacomdeta lfd
   WHERE ( mfe.plde_codigo = :ai_planta )
	  AND ( mfe.tpmv_codigo = :ai_tipmov )
	  AND ( mfe.mfco_numero = :al_numero )
	  AND ( mfe.plde_codigo = mfd.plde_codigo )
	  AND ( mfe.tpmv_codigo = mfd.tpmv_codigo )
	  AND ( mfe.mfco_numero = mfd.mfco_numero )
	  AND ( mfd.lofc_pltcod = lfd.lofc_pltcod )
	  AND ( mfd.lofc_espcod = lfd.lofc_espcod )
	  AND ( mfd.lofc_lotefc = lfd.lofc_lotefc )
	  AND ( mfd.lfcd_secuen = lfd.lfcd_secuen )
	  AND ( lfd.lofc_lotefc <> :al_lote )
	  AND ( lfd.enva_tipoen = :ai_tipoenva )
	  AND ( lfd.enva_codigo = :ai_envase );
	  
	IF SqlCa.SQLCode = -1 THEN
		F_ErrorBaseDatos(SqlCa, "Lectura de Tabla Movimientos")
      RETURN 0
	END IF 
	  
	IF isnull(ld_Bultos) THEN ld_Bultos=0
	
RETURN ld_Bultos	  
end function

public subroutine RecuperaEncab (integer ai_planta, integer ai_tipmovto, integer al_nummovto, integer ai_especie, integer ai_lote);
end subroutine

public function boolean rescatamovto (integer ai_planta, integer ai_tipdoc, long al_proceso);Long     li_movto

Select	mfc.mfco_numero
Into		:li_movto		
From   	dba.spro_movtofrutacomenca mfc
Where		mfc.plde_codigo 	=: ai_Planta
AND		mfc.mfco_tipdoc 	=: ai_Tipdoc
AND		mfc.mfco_docrel 	=: al_Proceso
AND 		clie_codigo 		=: iuo_cliente.Codigo;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_movtofrutacomenca")

	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN

	RETURN False
END IF

RETURN True
end function

public subroutine buscaproductor ();Str_Busqueda	lstr_Busq

OpenWithParm(w_busc_productores, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	dw_3.SetItem(1, "prod_codigo", Long(lstr_Busq.Argum[1]))
	dw_3.SetItem(1, "prod_nombre", lstr_Busq.Argum[2])

	dw_3.SetColumn("prod_codigo")
	dw_3.SetFocus()
END IF
end subroutine

public function boolean revisaenvases ();Long     ll_Fila, ll_FilaBusc, ll_Filaarr,arr_envases[500,4], ll_Row, &
         ll_FilaUlt, ll_loteac, ll_numero
Integer  li_TipoEnva, li_Envase, li_TipoEnvaSig, li_EnvaseSig, li_cont, &
         li_planta, li_tipmov, li_lotepl, li_lotees
Dec{2}   ld_sumbultos, ld_BuscaBultos=0
String   ls_mensaje
Boolean  lb_ya_ingresado = False

li_planta = dw_2.Object.plde_codigo[1]
li_tipmov = dw_2.Object.tpmv_codigo[1]
ll_numero = dw_2.Object.mfco_numero[1]
li_lotepl = dw_3.Object.lofc_espcod[1] 
li_lotees = dw_3.Object.lofc_pltcod[1]
ll_loteac = dw_3.Object.lofc_lotefc[1]

ll_Fila = 1
ll_FilaArr = 1

DO WHILE  ll_Fila<= dw_4.RowCount()
	
	ld_sumBultos = 0
	lb_ya_ingresado = False
	
   li_Tipoenva =	dw_4.Object.enva_tipoen[ll_Fila]
	li_envase	=	dw_4.Object.enva_codigo[ll_Fila]
		
	FOR ll_Row=1 To UpperBound(arr_envases)
		IF arr_envases[ll_row,1] = li_TipoEnva AND arr_envases[ll_row,2] = li_Envase THEN
			lb_ya_ingresado = TRUE
			ll_row = UpperBound(arr_envases)
		ELSE
			IF arr_envases[ll_row,1] = 0 THEN ll_row = UpperBound(arr_envases)
		END IF
	NEXT	
	
	IF lb_ya_ingresado = False THEN
		
		ld_SumBultos = dw_4.Object.lfcd_bultos[ll_Fila]
		
		FOR ll_FilaBusc = (ll_fila + 1) TO dw_4.RowCount()
			li_TipoEnvaSig =	dw_4.Object.enva_tipoen[ll_FilaBusc]
			li_EnvaseSig	=	dw_4.Object.enva_codigo[ll_FilaBusc]
			IF li_TipoEnva = li_TipoEnvaSig And li_Envase=li_EnvaseSig THEN
				ld_SumBultos = ld_SumBultos + dw_4.Object.lfcd_bultos[ll_FilaBusc]
			END IF	
		NEXT	
		
		ld_BuscaBultos = BuscaTotalEnvases(li_planta,li_tipmov,ll_numero, li_lotepl, &
		                 li_lotees, ll_loteac, li_tipoenva, li_envase )
		
		ld_SumBultos = ld_SumBultos + ld_BuscaBultos
		
		arr_envases[ll_FilaArr,1] = li_TipoEnva
		arr_envases[ll_FilaArr,2] = li_Envase
		arr_envases[ll_FilaArr,3] = ld_SumBultos
		arr_envases[ll_FilaArr,4] = 0
		ll_FilaArr++
		
	END IF
	ll_Fila++		
LOOP

ll_Fila = 1
DO WHILE  ll_Fila<= dw_5.RowCount()
	
	ld_sumBultos = 0
	lb_ya_ingresado = False
	
   li_Tipoenva =	dw_5.Object.enva_tipoen[ll_Fila]
	li_envase	=	dw_5.Object.enva_codigo[ll_Fila]
		
	FOR ll_Row=1 To UpperBound(arr_envases)
		IF arr_envases[ll_row,1] = li_TipoEnva AND arr_envases[ll_row,2] = li_Envase THEN
			lb_ya_ingresado = TRUE
			ll_row = UpperBound(arr_envases)
		ELSE
			IF arr_envases[ll_row,1] = 0 THEN ll_row = UpperBound(arr_envases)
		END IF
	NEXT	
	
	IF lb_ya_ingresado = False THEN
		
		ld_BuscaBultos = BuscaTotalEnvases(li_planta,li_tipmov,ll_numero, li_lotepl, &
		                 li_lotees, ll_loteac, li_tipoenva, li_envase )
		
		ld_SumBultos = ld_SumBultos + ld_BuscaBultos
		
		arr_envases[ll_FilaArr,1] = li_TipoEnva
		arr_envases[ll_FilaArr,2] = li_Envase
		arr_envases[ll_FilaArr,3] = ld_SumBultos
		arr_envases[ll_FilaArr,4] = 0
		ll_FilaArr++		
	END IF
	ll_Fila++		
LOOP

ll_Fila    = 1
ll_FilaUlt = ll_FilaArr

DO WHILE  ll_Fila<= dw_5.RowCount()
	
	ld_sumBultos = 0
	lb_ya_ingresado = False
	
   li_Tipoenva =	dw_5.Object.enva_tipoen[ll_Fila]
	li_envase	=	dw_5.Object.enva_codigo[ll_Fila]
		
	FOR ll_Row=1 To UpperBound(arr_envases)
		IF arr_envases[ll_Row,1] = li_TipoEnva AND arr_envases[ll_Row,2] = li_Envase THEN
			lb_ya_ingresado	=	TRUE
			ll_FilaArr			=	ll_Row
			ll_row				=	UpperBound(arr_envases)
		ELSE
			IF arr_envases[ll_Row,1] = 0 THEN ll_row	=	UpperBound(arr_envases)
		END IF
	NEXT	
	
	IF lb_ya_ingresado THEN
		
		IF arr_envases[ll_FilaArr,4] = 0 THEN
			ld_SumBultos = dw_5.Object.fgme_cantid[ll_Fila]
			
			FOR ll_FilaBusc = (ll_fila + 1) TO dw_5.RowCount()
				li_TipoEnvaSig	=	dw_5.Object.enva_tipoen[ll_FilaBusc]
				li_EnvaseSig	=	dw_5.Object.enva_codigo[ll_FilaBusc]
				IF li_TipoEnva = li_TipoEnvaSig And li_Envase=li_EnvaseSig THEN
					ld_SumBultos = ld_SumBultos + dw_5.Object.fgme_cantid[ll_FilaBusc]
				END IF	
			NEXT	
			
			arr_envases[ll_FilaArr,4] = ld_SumBultos
		END IF	
	ELSE
		arr_envases[ll_FilaUlt,1] = li_TipoEnva
		arr_envases[ll_FilaUlt,2] = li_Envase
		arr_envases[ll_FilaUlt,3] = 0
      arr_envases[ll_FilaUlt,4] = dw_5.Object.fgme_cantid[ll_Fila]
		ll_FilaUlt++
	END IF
	ll_Fila++
LOOP

FOR ll_Fila=1 TO UpperBound(arr_envases)
	IF arr_envases[ll_Fila,3]<>arr_envases[ll_Fila,4] THEN
   	li_cont ++
	   ls_mensaje = 	ls_mensaje + "~nTipo Envase " + String(arr_envases[ll_Fila,1]) 
		ls_mensaje = 	ls_mensaje + "  y Envase " + String(arr_envases[ll_Fila,2]) 
		ls_mensaje = 	ls_mensaje + "  Bultos (" + String(arr_envases[ll_Fila,3])
		ls_mensaje = 	ls_mensaje + ") (" + String(arr_envases[ll_Fila,4]) + ")"
	   IF arr_envases[ll_Fila,1] = 0 THEN ll_Fila = UpperBound(arr_envases)	
   END IF		
NEXT

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "No Existe Concordancia de Bultos en :" + ls_mensaje + ".", StopSign!, Ok!)
	//RETURN FALSE
END IF

RETURN TRUE
end function

public function boolean existeembalaje (integer ai_tipoenvase, integer ai_envase, string ls_columna);String	ls_codigo, ls_nombre
Integer	li_tipoen, li_codenvase

ls_codigo	= ls_columna

SELECT	emba_nombre, enva_tipoen, enva_codigo
	INTO 	:ls_nombre, :li_tipoen, :li_codenvase
	FROM	dba.embalajesprod
	WHERE emba_codigo	= 	:ls_codigo
	AND	enva_tipoen	= 	:ai_tipoenvase
	AND	enva_codigo	= 	:ai_envase
	AND 	clie_codigo =	:iuo_cliente.Codigo;
		
IF SqlCa.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Embalajes")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Embalaje (" + ls_codigo + &
					"), no ha sido creado en tabla respectiva.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
END IF

RETURN True

end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean		lb_AutoCommit, lb_Retorno
DataStore	lds_detalleMovto

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF NOT Borrando THEN
	IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		dw_2.SetItem(1,"mfco_usuari",gstr_us.nombre)
		dw_2.SetItem(1,"mfco_comput",gstr_us.computador)
		dw_2.SetItem(1,"mfco_horacr",F_FechaHora())
	ELSEIF dw_2.GetItemStatus(1, 0, Primary!) = DataModified! THEN
		dw_2.SetItem(1,"mfco_usumod",gstr_us.nombre)
		dw_2.SetItem(1,"mfco_commod",gstr_us.computador)
		dw_2.SetItem(1,"mfco_horact",F_FechaHora())
	END IF
END IF

lds_detalleMovto					=	Create DataStore
lds_detalleMovto.DataObject	=	dw_1.DataObject

lds_detalleMovto.SetTransObject(SQLCA)

IF dw_1.DeletedCount()>0 THEN
	//Vacía Buffer Delete! de Detalle
	dw_1.RowsMove(1,dw_1.DeletedCount(),Delete!,lds_detallemovto,1,Primary!)
	
	//Inicializa los Flags a NotModified! (Estaban en NewModified! al mover las filas)
	lds_detalleMovto.ResetUpdate()
	
	//Pasa las filas al Buffer Delete! para forzar una eliminación previa del detalle	
	lds_detalleMovto.RowsMove(1,lds_detalleMovto.RowCount(),Primary!,lds_detalleMovto,1,Delete!)
END IF

IF Borrando THEN
	IF dw_5.Update(True, False) = 1 THEN	
		IF dw_7.Update(True, False) = 1 THEN	
			IF lds_detalleMovto.Update(True,False) = 1 THEN
				IF dw_4.Update(True, False) = 1 THEN
					IF dw_6.Update(True, False) = 1 THEN
						IF dw_3.Update(True, False) = 1 THEN
							IF dw_2.Update(True, False) = 1 THEN
								Commit;
			
								IF sqlca.SQLCode <> 0 THEN
									F_ErrorBaseDatos(sqlca, This.Title)
								ELSE
									lb_Retorno	=	True
			
									dw_1.ResetUpdate()
									dw_2.ResetUpdate()
									dw_3.ResetUpdate()
									dw_4.ResetUpdate()
									dw_5.ResetUpdate()
									dw_6.ResetUpdate()
									dw_7.ResetUpdate()
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
ELSE
	IF dw_3.Update(True, False) = 1 THEN
		IF lds_detalleMovto.Update(True,False) = 1 THEN
			IF dw_4.Update(True, False) = 1 THEN
				IF dw_2.Update(True, False) = 1 THEN
					IF dw_1.Update(True, False) = 1 THEN
						IF dw_6.Update(True, False) = 1 THEN
							IF dw_5.Update(True, False) = 1 THEN
								IF dw_7.Update(True, False) = 1 THEN
									Commit;
				
									IF sqlca.SQLCode <> 0 THEN
										F_ErrorBaseDatos(sqlca, This.Title)
									ELSE
										lb_Retorno	=	True
			
										dw_1.ResetUpdate()
										dw_2.ResetUpdate()
										dw_3.ResetUpdate()
										dw_4.ResetUpdate()
										dw_5.ResetUpdate()
										dw_6.ResetUpdate()
										dw_7.ResetUpdate()
										
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
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	END IF		
// Nuevo Lote
//	IF (istr_Mant.Argumento[11] = "1" AND ( gstr_paramplanta.PoolVenta = 1 OR gstr_paramplanta.PoolVenta = 5 ) ) OR &
//		(istr_Mant.Argumento[11] = "2" AND ( gstr_Paramplanta.PoolRetiro = 1 OR gstr_Paramplanta.PoolRetiro = 4 ) ) THEN
//		dw_3.Reset()
//		dw_3.InsertRow(0)
//		dw_4.Reset()		
//		dw_3.Object.lofc_lotefc.Protect	=	0
//		dw_3.Object.lofc_lotefc.BackGround.Color = RGB(255,255,255)
//		dw_3.Object.prod_codigo.Protect	=	0
//		dw_3.Object.prod_codigo.BackGround.Color = RGB(255,255,255)
//		dw_3.Object.lofc_pltcod[1]	=	Integer(istr_Mant.Argumento[1])
//		dw_3.Object.lofc_espcod[1]	=	Integer(istr_Mant.Argumento[7])
//	
//		dw_3.Object.lofc_totbul[1] =	0
//		dw_3.Object.lofc_totkil[1] =	0
//	
//		dw_3.SetColumn("lofc_lotefc")
//		dw_3.SetFocus()
//		pb_grabar.Enabled = False
//		pb_eliminar.Enabled = False
//		pb_ins_det.Enabled = False
//		pb_eli_det.Enabled = False
//		
//	END IF
END IF

sqlca.AutoCommit	=	ib_AutoCommit

Destroy lds_detalleMovto

RETURN lb_Retorno
end function

public function boolean loteconmov (long al_lote);Integer	li_Planta, li_Especie, li_tipodoc, li_tipomovto
Long		ll_docrel, ll_numero

dw_2.AcceptText()

li_Planta		=	Integer(istr_Mant.Argumento[1])
li_Especie		=	Integer(istr_Mant.Argumento[7])
li_tipomovto   =  integer(istr_Mant.Argumento[2])

IF isnull(li_especie) or li_especie=0 THEN
	MessageBox("Atención", "Primero debe Seleccionar Una Orden de Proceso o un Numero de Movimiento.")
	RETURN FALSE
END IF		

SELECT lfcd_tipdoc, lfcd_docrel
  INTO :li_tipodoc, :ll_docrel
  FROM dba.spro_lotesfrutacomdeta
 WHERE lofc_pltcod =  :li_Planta
	and lofc_espcod =  :li_especie
	and lofc_lotefc =  :al_lote
	and lfcd_secuen =  1;
		
IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_lotesfrutacomdeta")
	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN
	RETURN FALSE
END IF

MessageBox("Atención","El lote " + String(al_lote) + " pertenece a otro proceso. ")
RETURN TRUE
end function

public function boolean existemovimiento (long al_numero);Integer	li_tiponum, li_planta, li_tipodocto
Long     ll_nrodocto
Date		ld_fecha

li_planta  = dw_2.Object.plde_codigo[1]
li_tiponum = Integer(istr_mant.argumento[2])

SELECT	mfco_tipdoc, mfco_docrel, mfco_fecmov
	INTO	:li_tipodocto, :ll_nrodocto, :ld_fecha
	FROM	dba.spro_movtofrutacomenca
	WHERE	plde_codigo	=	:li_planta
	AND	tpmv_codigo	=	:li_tiponum
	AND	mfco_numero	=	:al_numero
	AND 	clie_codigo =	:iuo_cliente.Codigo;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_movtofrutacomenca")

	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN

	RETURN False
END IF

istr_mant.argumento[4] = string(li_tipodocto)
istr_mant.argumento[5] = String(ll_nrodocto)
dw_2.Object.mfco_fecmov[1] = ld_fecha

RETURN True
end function

public function integer tiporecepcion (string as_recepcion, date adt_fechamovto);Integer li_lote

IF as_Recepcion = '1' THEN
	CHOOSE CASE gstr_paramplanta.PoolVenta
		CASE 1
			li_Lote = 0
			
		CASE 2
			li_lote = f_SemanaFecha(datetime(adt_fechamovto))

		CASE 3
			li_lote = f_DiaFecha(datetime(adt_fechamovto))

		CASE 4
			li_lote = f_anoFecha(datetime(adt_fechamovto))			
			
		CASE 5
			li_Lote = 0
			
	END CHOOSE
		
ELSEIF as_Recepcion = '2' THEN
	CHOOSE CASE gstr_paramplanta.PoolRetiro
		CASE 1
			li_Lote = 0
				
		CASE 2
			li_Lote = Integer(istr_Mant.Argumento[5])			
		
		CASE 3
			li_Lote = f_Semanafecha(datetime(adt_fechamovto))	

		CASE 4
			li_Lote = 0	
			
	END CHOOSE
END IF			
 
RETURN li_Lote
end function

public function boolean buscamovto (integer ai_tipdoc, long al_docrel);Long     ll_nrodocto
Integer  li_planta, li_tiponum
Date		ld_fecha

li_planta  = dw_2.Object.plde_codigo[1]
li_tiponum = Integer(istr_mant.argumento[2])
 
SELECT	mfco_numero, mfco_fecmov
	INTO	:ll_nrodocto, :ld_fecha
	FROM	dbo.spro_movtofrutacomenca
	WHERE	plde_codigo	=	:li_planta
	AND	tpmv_codigo	=	:li_tiponum
	AND	mfco_tipdoc	=	:ai_tipdoc
	AND   mfco_docrel =  :al_docrel
	AND 	clie_codigo = 	:iuo_cliente.Codigo;


IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_movtofrutacomenca")

	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN

	RETURN False
END IF

istr_Mant.Argumento[3] = string(ll_nrodocto)
istr_mant.argumento[4] = string(ai_tipdoc)
istr_mant.argumento[5] = String(al_docrel)
dw_2.Object.mfco_fecmov[1] = ld_fecha

RETURN True
end function

public subroutine habilitaingreso (string as_columna);Date		ld_fecha
Boolean	lb_estado = True

dw_2.AcceptText()
dw_3.AcceptText()

	IF as_Columna <> "plde_codigo" AND &
		(dw_2.Object.plde_codigo[1] = 0 OR IsNull(dw_2.Object.plde_codigo[1])) THEN
		lb_Estado = False
	END IF

	IF as_Columna <> "mfco_fecmov" AND &
		(dw_2.Object.mfco_fecmov[1] = ld_Fecha OR IsNull(dw_2.Object.mfco_fecmov[1])) THEN
		lb_Estado = False
	END IF
	
	IF as_Columna <> "mfco_tipdoc" AND &
		(dw_2.Object.mfco_tipdoc[1] = 0 OR IsNull(dw_2.Object.mfco_tipdoc[1])) THEN
		lb_Estado = False
	END IF
		
	IF as_Columna <> "mfco_docrel" AND &
		(dw_2.Object.mfco_docrel[1] = 0 OR IsNull(dw_2.Object.mfco_docrel[1])) THEN
		lb_Estado = False
	END IF
	

pb_ins_det.Enabled = lb_estado
tab_1.tp_1.Enabled = lb_estado
tab_1.tp_2.Enabled = lb_estado
end subroutine

public function boolean existelote (long al_lote);Integer	li_Cantidad, li_Planta, li_Especie, li_tipomovto, li_camara, &
			li_tipoenva, li_envase, li_tipodoc, li_secuencia
Long		ll_secuen, ll_numero, ll_lote, ll_docrel
Double   ld_bultos,ld_kilos

dw_2.AcceptText()

li_Planta		=	Integer(istr_Mant.Argumento[1])
li_Especie		=	Integer(istr_Mant.Argumento[7])
li_tipomovto   =  integer(istr_Mant.Argumento[2])
ll_numero 	 	=  integer(istr_Mant.Argumento[3])

IF isnull(li_especie) or li_especie=0 THEN
	MessageBox("Atención", "Primero debe Seleccionar Una Orden de Proceso o un Numero de Movimiento.")
	RETURN FALSE
END IF		

IF al_lote <> 0 THEN
	SELECT	Max(lfcd_secuen)
		INTO	:li_secuencia
		FROM	dba.spro_movtofrutacomdeta
		WHERE	lofc_pltcod	=	:li_Planta
		AND	lofc_espcod =  :li_especie
		AND	lofc_lotefc =  :al_lote
		AND	tpmv_codigo =  :li_tipomovto
		AND	mfco_numero	=  :ll_numero
		AND 	clie_codigo = 	:iuo_cliente.Codigo;
  	
	SELECT	mfcd_bulent, mfcd_kgnent
		INTO	:ld_bultos, :ld_kilos 
		FROM	dba.spro_movtofrutacomdeta
		WHERE	lofc_pltcod	=	:li_Planta
		AND	lofc_espcod =  :li_especie
		AND	lofc_lotefc =  :al_lote
		AND	tpmv_codigo =  :li_tipomovto
		AND	mfco_numero =  :ll_numero
		AND	lfcd_secuen =  :li_secuencia
		AND 	clie_codigo	=	:iuo_cliente.Codigo;
  
	IF SqlCa.SQLCode = -1 THEN
		F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_lotesfrutacomdeta")
		RETURN False
	ELSEIF SqlCa.SQLCode = 100 THEN
		RETURN FALSE
	END IF
	
	dw_3.Object.lofc_totbul[1] = ld_bultos
	dw_3.Object.lofc_totkil[1] = ld_Kilos
				 
	IF dw_3.Retrieve(li_planta, li_Especie, al_lote) =  -1 THEN
		MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.")
		RETURN FALSE
		
	 ELSEIF dw_4.Retrieve(li_planta,li_especie, al_lote) = -1 AND &
			  dw_5.Retrieve(li_planta,li_tipomovto, ll_numero,1, iuo_cliente.Codigo) = -1 THEN
			  
				MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.")
				RETURN FALSE
					
	 ELSE
			IF dw_4.Rowcount() > 0 THEN
				dw_4.SetRedraw(False)
				
				pb_grabar.Enabled   = TRUE
				pb_imprimir.Enabled = TRUE
				
				li_tipodoc	=	dw_2.Object.mfco_tipdoc[1]
				ll_docrel	=	dw_2.Object.mfco_docrel[1]

				dw_4.SetFilter("")
				dw_4.Filter()

				dw_4.SetFilter("lfcd_tipdoc = " + String(li_tipodoc) + " And lfcd_docrel = " + String(ll_docrel))
				dw_4.Filter()
								
				il_fila = 1
				dw_4.SetRow(1)
				dw_4.SetFocus()
				dw_4.SetRedraw(TRUE)
			END IF
			RETURN TRUE
	 END IF
ELSE
	
  SELECT	IsNull(Max(lofc_lotefc), 0)
	 INTO	:ll_lote
	 FROM	dba.spro_movtofrutacomdeta
	WHERE	plde_codigo =  :li_Planta
	  AND lofc_pltcod	=	:li_Planta
	  and lofc_espcod =  :li_especie
	  and tpmv_codigo =  :li_tipomovto
	  and mfco_numero =  :ll_numero
	  and clie_codigo	=	:iuo_cliente.Codigo;
	
	IF ll_Lote <> 0 THEN
		Istr_Mant.Argumento[6] = String(ll_lote)
		RETURN TRUE	
	ELSE
		RETURN FALSE
	END IF
END IF
end function

public function long nuevofoliolote ();Long ll_Folio, ll_planta, ll_especie

ll_planta	=	Long(istr_Mant.Argumento[1])
ll_especie	=	Long(istr_Mant.Argumento[7])

SELECT	MAX(lofc_lotefc)
INTO		:ll_Folio
FROM		dba.spro_lotesfrutacomenc
WHERE		lofc_pltcod = :ll_planta
AND		lofc_espcod = :ll_especie;

ll_Folio	=	ll_Folio + 1

RETURN ll_Folio
end function

public subroutine habilitalote ();
IF Istr_Mant.Argumento[11] = "1" AND (gstr_paramplanta.PoolVenta = 1 OR &
												  gstr_paramplanta.PoolVenta = 5) THEN
   dw_3.Object.lofc_lotefc.Protect	=	0 
	dw_3.Object.lofc_lotefc.BackGround.Color = RGB(255,255,255)
	
//ELSEIF Istr_Mant.Argumento[11] = "2" AND (gstr_paramplanta.PoolRetiro = 1 OR &
//												      gstr_paramplanta.PoolRetiro = 4) THEN
//   dw_3.Object.lofc_lotefc.Protect	=	0
//	dw_3.Object.lofc_lotefc.BackGround.Color = RGB(255,255,255)

ELSEIF Istr_Mant.Argumento[11] = "2" AND (gstr_paramplanta.PoolRetiro = 1 OR &
												      gstr_paramplanta.PoolRetiro = 4) THEN
   dw_3.Object.lofc_lotefc.Protect	=	1
	dw_3.Object.lofc_lotefc.BackGround.Color = rgb(166,180,210)

ELSE
   dw_3.Object.lofc_lotefc.Protect	=	1
	dw_3.Object.lofc_lotefc.BackGround.Color = rgb(166,180,210)
END IF
	
end subroutine

public function long nuevolote (long al_revlote);Integer li_planta, li_especie, Existelote=1
Long    ll_lote=0

li_planta  = Integer(istr_mant.Argumento[1])
li_especie = Integer(istr_mant.Argumento[7])

IF isnull(al_revlote) = False AND al_revlote > 0 THEN
	  
	 SELECT Count(*) INTO :ExisteLote 
	   FROM dba.spro_lotesfrutacomenc 
 	  WHERE lofc_pltcod = :li_planta
		 AND lofc_espcod = :li_especie
		 AND lofc_lotefc = :al_revlote;
		 
END IF

IF ExisteLote = 0 OR isnull(existelote) THEN
	
	ll_Lote = al_RevLote
	dw_3.Object.lofc_pltcod[1] = li_planta
	dw_3.Object.lofc_espcod[1] = li_especie
	
	iuo_prodempresa.existe(gstr_paramplanta.productorempresa,false,sqlca)
	dw_3.SetItem(1, "prod_codigo", gstr_paramplanta.productorempresa)
	dw_3.SetItem(1, "prod_nombre", iuo_prodempresa.nombre )

	dw_3.Object.lofc_totbul[1] = 0
	dw_3.Object.lofc_totkil[1] = 0
ELSE
	IF istr_mant.Argumento[11] = "1" THEN
		IF gstr_paramplanta.PoolVenta = 5 OR gstr_paramplanta.PoolVenta = 1 THEN
			SELECT loco_ultcom  into :ll_lote
			  FROM dba.spro_lotescorrel
			 WHERE espe_codigo = :li_especie
				AND plde_codigo = :li_planta;
		ELSE
			SELECT loco_inicom  into :ll_lote
	  		  FROM dba.spro_lotescorrel
	 		 WHERE espe_codigo = :li_especie
				AND plde_codigo = :li_planta;
		END IF

	ELSE
		IF gstr_paramplanta.PoolRetiro = 4 OR gstr_paramplanta.PoolRetiro = 1 THEN
			SELECT loco_ulcore  into :ll_lote
			  FROM dba.spro_lotescorrel
			 WHERE espe_codigo = :li_especie
				AND plde_codigo = :li_planta;
		ELSE
			SELECT loco_incore  into :ll_lote
			  FROM dba.spro_lotescorrel
			 WHERE espe_codigo = :li_especie
				AND plde_codigo = :li_planta;
		END IF
	END IF
	
	dw_3.Object.lofc_pltcod[1] = li_planta
	dw_3.Object.lofc_espcod[1] = li_especie
	
	iuo_prodempresa.existe(gstr_paramplanta.productorempresa,false,sqlca)
	dw_3.SetItem(1, "prod_codigo", gstr_paramplanta.productorempresa)
	dw_3.SetItem(1, "prod_nombre", iuo_prodempresa.nombre )

	dw_3.Object.lofc_totbul[1] = 0
	dw_3.Object.lofc_totkil[1] = 0
	
	IF isnull(ll_lote) THEN ll_lote=0
	IF (Istr_Mant.Argumento[11] = "1" AND (gstr_Paramplanta.PoolVenta = 1) OR &
													  (gstr_Paramplanta.PoolVenta = 5)) OR &
		(Istr_Mant.Argumento[11] = "2" AND (gstr_Paramplanta.PoolRetiro = 1) OR &
													  (gstr_Paramplanta.PoolRetiro = 4)) THEN
		ll_lote = ll_lote + 1
	END IF
	
	IF (istr_Mant.Argumento[11] = "1" AND (gstr_paramplanta.PoolVenta = 1) OR &
													  (gstr_paramplanta.PoolVenta = 5)) THEN
		UPDATE dba.spro_lotescorrel SET loco_ultcom = :ll_lote
		  FROM dba.spro_lotescorrel 
		 WHERE espe_codigo = :li_especie
			AND plde_codigo = :li_planta;
	ELSEIF gstr_paramplanta.PoolRetiro = 1 OR gstr_paramplanta.PoolRetiro = 4 THEN
		UPDATE dba.spro_lotescorrel SET loco_ulcore = :ll_lote
        FROM dba.spro_lotescorrel 
		 WHERE espe_codigo = :li_especie
			AND plde_codigo = :li_planta;
	END IF
	
	pb_grabar.enabled=false
	pb_eliminar.enabled=false

END IF

RETURN ll_lote
end function

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect	=	0 
	dw_2.Object.clie_codigo.BackGround.Color = RGB(255,255,255)
   dw_2.Object.plde_codigo.Protect	=	0 
	dw_2.Object.plde_codigo.BackGround.Color = RGB(255,255,255)
	dw_2.Object.mfco_numero.Protect	=	0
	dw_2.Object.mfco_numero.BackGround.Color = RGB(255,255,255)
	dw_2.Object.mfco_fecmov.Protect	=	0
	dw_2.Object.mfco_fecmov.BackGround.Color = RGB(255,255,255)
	dw_2.Object.mfco_tipdoc.Protect	=	0
	dw_2.Object.mfco_tipdoc.BackGround.Color = RGB(255,255,255)
	dw_2.Object.mfco_docrel.Protect	=	0
	dw_2.Object.mfco_docrel.BackGround.Color = RGB(255,255,255)
   dw_3.Object.lofc_lotefc.Protect	=	0 
	dw_3.Object.lofc_lotefc.BackGround.Color = RGB(255,255,255)
	
	dw_2.Object.b_ordenproceso.visible = 1

	
//	IF istr_Mant.Argumento[11] = "1" AND gstr_paramplanta.poolventa = 5 THEN
//		dw_3.Object.b_nuevolote.visible  =  1	
//	ELSEIF istr_Mant.Argumento[11] = "2" AND gstr_paramplanta.poolretiro = 4 THEN
//			 dw_3.Object.b_nuevolote.visible  =  1	
//	END IF
 
	dw_2.SetFocus()
ELSE
	dw_2.Object.clie_codigo.Protect	=	1 
	dw_2.Object.clie_codigo.BackGround.Color = rgb(166,180,210)
   dw_2.Object.plde_codigo.Protect	=	1
	dw_2.Object.plde_codigo.BackGround.Color = rgb(166,180,210)
	dw_2.Object.mfco_numero.Protect	=	1
	dw_2.Object.mfco_numero.BackGround.Color = rgb(166,180,210)
	dw_2.Object.mfco_fecmov.Protect	=	1
	dw_2.Object.mfco_fecmov.BackGround.Color = rgb(166,180,210)
	dw_2.Object.mfco_tipdoc.Protect	=	1
	dw_2.Object.mfco_tipdoc.BackGround.Color = rgb(166,180,210)
	dw_2.Object.mfco_docrel.Protect	=	1
	dw_2.Object.mfco_docrel.BackGround.Color = rgb(166,180,210)

	dw_2.Object.b_ordenproceso.visible = 0
	
//	IF istr_Mant.Argumento[11] = "1" AND (gstr_paramplanta.poolventa = 1 OR &
//		gstr_paramplanta.poolventa = 5) THEN
//		dw_3.Object.b_nuevolote.visible  =  1	
//	ELSEIF istr_Mant.Argumento[11] = "2" AND (gstr_paramplanta.poolretiro = 1 OR &
//			 gstr_paramplanta.poolretiro = 4) THEN
//			 dw_3.Object.b_nuevolote.visible  =  1	
//	END IF
	
END IF
end subroutine

public function boolean borradetallemovto (integer ai_planta, integer ai_especie, long al_lote, integer ai_secuen);Long ll_Fila, ll_borra

ll_Borra=0
ll_fila=1

Do While ll_fila<=dw_1.RowCount()
	
	IF  dw_1.Object.lofc_pltcod[ll_fila] = ai_planta  AND &
	    dw_1.Object.lofc_espcod[ll_fila] = ai_especie AND &
		 dw_1.Object.lofc_lotefc[ll_fila] = al_lote    AND &
		 dw_1.Object.lfcd_secuen[ll_fila] = ai_secuen  THEN
		 
		 IF dw_1.deleterow(ll_fila) = 1 THEN
			ll_borra++
		 END IF
		
	ELSE
		ll_fila++
	END IF	
	
loop	

IF ll_Borra>0 THEN
	RETURN TRUE
ELSE
	RETURN FALSE
END IF	
end function

public subroutine buscaordenproceso ();Str_Busqueda	lstr_Busq
Integer	ll_lote

lstr_Busq.Argum[1]	=	istr_Mant.Argumento[1]	// Planta
lstr_Busq.Argum[2]	=	'1' 							// Estado = Vigente O. Proceso
lstr_Busq.Argum[3]	=	istr_Mant.Argumento[4]
lstr_Busq.Argum[16]	=	string(iuo_cliente.Codigo)

OpenWithParm(w_busc_spro_ordenproceso, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	dw_2.SetItem(1, "mfco_tipdoc", Integer(lstr_Busq.Argum[10]))
	dw_2.SetItem(1, "mfco_docrel", Long(lstr_Busq.Argum[6]))

	IF iuo_spro_ordenproceso.Existe(Integer(istr_Mant.Argumento[1]), &
											  Integer(lstr_Busq.Argum[10]), &
											  Long(lstr_Busq.Argum[6]), True, SqlCa, iuo_cliente.Codigo) THEN
						  
		istr_Mant.Argumento[5]	= lstr_Busq.Argum[6]
		istr_Mant.Argumento[7]	= String(iuo_spro_ordenproceso.Especie)
		istr_Mant.Argumento[12] = Mid(String(iuo_spro_ordenproceso.FechaOrden),1,10)		

		iuo_spro_ordenproceso.CapturaReferencias(1,0,1,0,0,False,Sqlca)

		dw_2.SetItem(1, "prod_codigo", iuo_spro_ordenproceso.productor)
		dw_2.SetItem(1, "prod_nombre", iuo_spro_ordenproceso.NombreProductor)
		dw_2.SetItem(1, "frio_tipofr", iuo_spro_ordenproceso.TipoFrio)
		dw_2.SetItem(1, "pefr_codigo", iuo_spro_ordenproceso.PeriodoFrio)
		dw_2.SetItem(1, "espe_codigo", iuo_spro_ordenproceso.Especie)
		dw_2.SetItem(1, "vari_codigo", iuo_spro_ordenproceso.variedad)
		dw_2.SetItem(1, "vari_nombre", iuo_spro_ordenproceso.NombreVariedad)
		
		dw_3.SetItem(1, "lofc_espcod", iuo_spro_ordenproceso.Especie)
		
		iuo_prodempresa.existe(gstr_paramplanta.productorempresa,false,sqlca)
		
		dw_3.SetItem(1, "prod_codigo", gstr_paramplanta.productorempresa)
		dw_3.SetItem(1, "prod_nombre", iuo_prodempresa.nombre )
		
		IF (istr_Mant.Argumento[11] = "1" AND gstr_paramplanta.PoolVenta <> 1 AND &
														 gstr_paramplanta.PoolVenta <> 5) OR &
			(istr_Mant.Argumento[11] = "2" AND gstr_Paramplanta.PoolRetiro <> 1 AND &
														 gstr_Paramplanta.PoolRetiro <> 4) THEN
			Istr_Mant.Argumento[6] = String(Nuevolote(0) + TipoRecepcion(istr_Mant.Argumento[11], &
													  Date(Mid(String(iuo_spro_ordenproceso.FechaOrden),1,10))))
			dw_3.Object.lofc_lotefc[1] = nuevofoliolote()//Long(Istr_Mant.Argumento[6])		
			pb_ins_det.Enabled = TRUE
			tab_1.tp_1.Enabled = TRUE
			tab_1.tp_2.Enabled = TRUE
		END IF
		
		
		IF (Istr_Mant.Argumento[11] = "1") AND &
				 gstr_paramplanta.PoolVenta = 5 OR gstr_paramplanta.PoolVenta = 1 THEN 
				 dw_3.Object.lofc_lotefc[1] = NuevoFolioLote() //Integer("")
				 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
		ELSEIF (Istr_Mant.Argumento[11] = "2") AND &
				 gstr_paramplanta.PoolRetiro = 1 OR gstr_paramplanta.PoolRetiro = 4 THEN 
				 dw_3.Object.lofc_lotefc[1] = Integer("")
				 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
		ELSE
				 dw_3.Object.lofc_lotefc[1] = Integer(Istr_Mant.Argumento[6])
				 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
				 Habilitaencab(False)
				 dw_5.Retrieve(Integer(istr_Mant.Argumento[1]), Integer(istr_Mant.Argumento[2]),&
				 							  Long(istr_Mant.Argumento[3]),1, iuo_cliente.Codigo)
				 
		END IF
		
		
		dw_3.Enabled = True
		HabilitaLote()

		IF buscamovto(Integer(lstr_Busq.Argum[10]),Long(lstr_Busq.Argum[6])) THEN
  			IF Existelote(0) THEN TriggerEvent("ue_recuperadatos")
		END IF

		
	END IF

	istr_Mant.Argumento[5]	= lstr_Busq.Argum[6]
	istr_Mant.Argumento[7]	= String(iuo_spro_ordenproceso.Especie)

	dw_2.SetColumn("mfco_numero")
	dw_2.SetFocus()
	pb_ins_det.Enabled = True
	tab_1.tp_1.Enabled = True
	tab_1.tp_2.Enabled = True
END IF
end subroutine

public function boolean retiraprod (long al_productor, integer ai_especie, integer ai_variedad, integer ai_categoria, date ad_fechalot);Long ll_Productor

SELECT   prod_codigo
INTO		:ll_productor
FROM		dba.spro_pararetiroprod
WHERE		prod_codigo =: al_productor
AND		espe_codigo =: ai_especie
AND		vari_codigo =: ai_variedad
AND      cate_codigo =: ai_categoria
AND      prep_fecini <: ad_fechalot;

IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla parametros de retiro productor")

	RETURN False
ELSEIF SqlCa.SQLCode = 100 THEN

	RETURN False
END IF

RETURN True

end function

public subroutine asignaclientea_dws ();integer li_i

for li_i = 1 to dw_1.RowCount()
	dw_1.object.clie_codigo[li_i] = iuo_cliente.Codigo
next

for li_i = 1 to dw_2.RowCount()
	dw_2.object.clie_codigo[li_i] = iuo_cliente.Codigo
next

for li_i = 1 to dw_5.RowCount()
	dw_5.object.clie_codigo[li_i] = iuo_cliente.Codigo
next

for li_i = 1 to dw_6.RowCount()
	dw_6.object.clie_codigo[li_i] = iuo_cliente.Codigo
next

for li_i = 1 to dw_7.RowCount()
	dw_7.object.clie_codigo[li_i] = iuo_cliente.Codigo
next
end subroutine

public function boolean existetarja (integer cliente, integer planta, long tarja);Long	 	ll_fila
Boolean	lb_Retorno

ll_fila = dw_4.Find("fgmb_nrotar = " + String(tarja) , 1, dw_1.RowCount())		

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error", "La tarja digitada a pertenece a este Proceso.", Exclamation!)
	lb_Retorno	=	TRUE
END IF


//IF NOT lb_Retorno THEN
//	SELECT	fgmb_nrotar
//		INTO	:ll_fila
//		FROM	dba.spro_movtobins
//		WHERE	clie_codigo=	:cliente
//		AND	plde_codigo	=	:planta
//		AND  	fgmb_nrotar	=	:tarja;	
//	
//	IF sqlca.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_Bins")
//	
//		lb_Retorno	=	TRUE
//	ELSEIF sqlca.SQLCode = 100 THEN
//		lb_Retorno	=	FALSE
//	ELSE
//		MessageBox("Error", "La tarja digitada a pertenece a otro Proceso.", Exclamation!)
//		lb_Retorno	= TRUE
//	END IF
//END IF

RETURN lb_Retorno
end function

public function decimal obtienekilosant (long al_tarja);decimal	ld_kilos, ld_tara
Long		ll_Bins, ll_Cliente, ll_lote

ll_Cliente = dw_2.Object.clie_codigo[1]
ll_Bins	=	dw_4.Object.bins_numero[il_fila] 
ll_lote	=	dw_4.Object.lofc_lotefc[il_fila] 

SELECT sum(lfcd_kilpro)
INTO :ld_kilos
FROM dba.spro_lotesfrutacomdeta AS lcd,
		dba.spro_movtobins as mvb,
		dba.spro_bins as bin
WHERE mvb.fgmb_nrotar = lcd.fgmb_nrotar
		AND lcd.lofc_lotefc 	<>	:ll_lote
		AND bin.clie_codigo 	= 	mvb.clie_codigo
		AND bin.plde_codigo 	= 	mvb.plde_codigo
    	AND bin.bins_numero 	= 	mvb.bins_numero 
		AND fgmb_estado 		= 	2
		AND mvb.fgmb_nrotar 	= 	:al_Tarja
		AND mvb.bins_numero 	= 	:ll_bins;
		
IF SqlCa.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla SPRO_MOVTOBINS")
	RETURN 0
ELSEIF IsNull(ld_kilos) THEN
	ld_kilos = 0
END IF

SELECT cale_pesoen
INTO :ld_tara
FROM dba.spro_calicosechero as cal,
		dba.spro_bins as bin
WHERE cal.enva_tipoen = bin.enva_tipoen
		AND cal.enva_codigo = bin.enva_codigo
		AND cal.cale_calida = bin.cale_calida
		AND bin.bins_numero = :ll_Bins
		AND bin.clie_codigo = :ll_Cliente;

IF SqlCa.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla SPRO_CaliCosechero")
	RETURN 0
ELSEIF IsNull(ld_tara) THEN
	MessageBox("Advertencia", "Nro de bins no ha sido ingresado en el sistema, por lo tanto no se realiza el destare")
	ld_tara = 0
END IF

RETURN ld_kilos + ld_tara

end function

public function boolean destarafila (long al_fila);Decimal	ld_TaraEnvase
Integer	li_encuentra
String		ls_calidad


ls_calidad = dw_4.Object.cale_calida[al_fila]

IF IsNull(ls_calidad) or ls_calidad = "" THEN Return False

ld_TaraEnvase	=	( iuo_calidad.Peso * dw_4.Object.lfcd_bultos[al_fila] ) + dw_4.Object.lfcd_kilobp[al_fila]

dw_4.Object.lfcd_kilnet[al_fila]	=	dw_4.Object.lfcd_kilbru[al_fila] - ld_TaraEnvase

dw_4.Object.lfcd_kilpro[al_fila]	=	dw_4.Object.lfcd_kilnet[al_fila] / dw_4.Object.lfcd_bultos[al_fila]

Return True
end function

public subroutine buscacalidad ();integer 			li_envase, li_tipoenvase
str_busqueda	lstr_mant

lstr_mant.argum[1]	=	String(dw_4.Object.enva_tipoen[il_fila])
lstr_mant.argum[2]	= 	String(dw_4.Object.enva_codigo[il_fila])
lstr_mant.argum[3]	= '*'

OpenWithParm(w_busc_calicosechero, lstr_mant)

lstr_mant				=	Message.PowerObjectParm

IF UpperBound(lstr_mant.argum) < 4 THEN RETURN

dw_4.Object.cale_calida[il_fila]		=	lstr_mant.argum[3]
dw_4.Object.cale_nombre[il_fila]	=	lstr_mant.argum[4]
end subroutine

on w_maed_movtofrutacomer_granel.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.tab_1=create tab_1
this.em_kilos=create em_kilos
this.ole_puerta=create ole_puerta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.tab_1
this.Control[iCurrent+3]=this.em_kilos
this.Control[iCurrent+4]=this.ole_puerta
end on

on w_maed_movtofrutacomer_granel.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.tab_1)
destroy(this.em_kilos)
destroy(this.ole_puerta)
end on

event open;/*
Argumentos :	[1]		=>	Código de Planta
					[2]		=>	Tipo de Movimiento
					[3]		=>	Numero de Movimiento
					[4]		=>	Tipo de Proceso
					[5]		=>	Numero de Proceso
					[6]		=>	Folio Lote
					[7]		=>	Código de Especie
					[8]		=>	Tipo de Envase
					[9]		=>	Código de Envase
					[10]	=>	Código de Camara
					[11]  	=> 	Tipo de Recepcion (1-Venta/ 2-Retiro)
					[12]  	=> 	Fecha Movto. Orden Proceso
*/

Integer 	li_resultado
String		ls_parametros
Boolean	ib_OCX	=	True

x	= 0
y	= 0
iuo_cliente							=	Create uo_clientesprod
iuo_variedades						=	Create uo_variedades
iuo_categorias						=	Create uo_categorias
iuo_planta							=	Create uo_plantadesp
iuo_productores					=	Create uo_productores
iuo_prodempresa					=	Create uo_productores
iuo_spro_ordenproceso			=	Create uo_spro_ordenproceso
iuo_TipoMovtoFruta				=	Create uo_tipomovtofruta
iuo_TipoMovtoEnva				=  Create uo_tipomovtofruta		
iuo_bins								=	Create uo_bins
iuo_calidad							=	Create uo_calicosechero

This.Height							= 2020
im_menu								= m_principal

This.Icon										=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

IF gstr_paramplanta.binsabins THEN
	tab_1.tp_1.dw_detalle.DataObject = "dw_mues_lotesfrutacomdeta_proceso_bins"
ELSEIF gstr_paramplanta.palletdebins then
	tab_1.tp_1.dw_detalle.DataObject = "dw_mues_lotesfrutacomdeta_proceso_bp"
ELSE
	tab_1.tp_1.dw_detalle.DataObject = "dw_mues_lotesfrutacomdeta_proceso"
END IF
dw_4	=	tab_1.tp_1.dw_detalle
dw_5	=	tab_1.tp_2.dw_envases
dw_6	=	Create DataStore
dw_7	=	Create DataStore

dw_6.DataObject	=	"dw_mant_movtoenvaenca_comercial" //cliente
dw_7.DataObject	=	"dw_mues_movtoenvadeta"

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_4.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

dw_2.GetChild("plde_codigo", idwc_planta)
dw_2.GetChild("clie_codigo", idwc_cliente)
dw_2.GetChild("espe_codigo", idwc_especie)
dw_2.GetChild("frio_tipofr", idwc_tipofrio)
dw_2.GetChild("pefr_codigo", idwc_periodofrio)
dw_3.GetChild("prod_codigo", idwc_productor)
dw_4.GetChild("cate_codigo", idwc_categoria)
dw_4.GetChild("enva_tipoen", idwc_tipoenvase)
dw_4.GetChild("cale_calida", idwc_calidad)

idwc_planta.SetTransObject(SqlCa)
idwc_cliente.SetTransObject(SqlCa)
idwc_especie.SetTransObject(SqlCa)
idwc_tipofrio.SetTransObject(SqlCa)
idwc_periodofrio.SetTransObject(SqlCa)
idwc_productor.SetTransObject(SqlCa)
idwc_categoria.SetTransObject(SqlCa)
idwc_tipoenvase.SetTransObject(SqlCa)
idwc_calidad.SetTransObject(SqlCa)

idwc_planta.Retrieve()
IF idwc_especie.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Especie")
	idwc_especie.InsertRow(0)
END IF

IF idwc_tipofrio.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Tipo de Frío")
	idwc_tipofrio.InsertRow(0)
END IF

IF idwc_periodofrio.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Periodo de Frío")
	idwc_periodofrio.InsertRow(0)
END IF

IF idwc_productor.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Productores")
	idwc_productor.InsertRow(0)
ELSE
	idwc_productor.SetSort("prod_nombre A")
	idwc_productor.Sort()
END IF

IF idwc_categoria.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Categoría")
	idwc_categoria.InsertRow(0)
ELSE
	idwc_categoria.SetSort("cate_nombre A")
	idwc_categoria.Sort()

	idwc_categoria.SetFilter("cate_embala <> 1")
	idwc_categoria.Filter()
END IF

IF idwc_tipoenvase.Retrieve() = 0 THEN
	MessageBox("Atención", "Falta Registrar Tipo de Envase")
	idwc_tipoenvase.InsertRow(0)
ELSE
	idwc_TipoEnvase.SetSort("tien_nombre A")
	idwc_TipoEnvase.Sort()
	
	idwc_TipoEnvase.SetFilter("tien_usoenv = 1")
	idwc_TipoEnvase.Filter() 
END IF

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

istr_Mant.Argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)	// Planta
istr_Mant.Argumento[2]	=	"9"												// Tipo Movto. (Recep. de Proceso)
istr_Mant.Argumento[3]	=	""
istr_Mant.Argumento[4]	=	"4"												// Tipo de Proceso (Proceso)
istr_Mant.Argumento[11] =  Message.StringParm							// Tipo de Recepcion (Venta/Retiro)

buscar	= "Turno:Nlfcd_turno,Tipo Envase:Nenva_tipoen,Envase:Nenva_codigo,Calibre:Srefe_calibr"
ordenar	= "Turno:lfcd_turno,Tipo Envase:enva_tipoen,Envase:enva_codigo,Calibre:refe_calibr"

IF gstr_paramplanta.binsabins = False THEN
	em_kilos.Visible	=	False
END IF

li_resultado 	=	ConfiguracionPuerta(istr_puertacomm)

IF li_resultado = 0 THEN
	ls_parametros	=	String(istr_puertacomm.Baudios)+","+&
							istr_puertacomm.Paridad+","+&
							String(istr_puertacomm.Data)+","+&
							String(istr_puertacomm.Parada)
			
	IF NOT ib_OCX THEN
		MessageBox("Conexión Romana","No está instalado el OCX para conexión con Romana")
	ELSE
		IF Ole_puerta.object.PortOpen THEN Ole_puerta.object.PortOpen = False
		Ole_puerta.object.settings	=	ls_parametros
		Ole_puerta.object.PortOpen	= True	
		Timer(0.2)
	END IF
END IF
end event

event ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta,  ll_docrel, ll_control
Integer li_tipodoc
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	dw_3.SetRedraw(False)
	dw_3.Reset()
   dw_4.SetRedraw(False)
	IF dw_2.Retrieve(Integer(istr_Mant.Argumento[1]), &
						  Integer(istr_Mant.Argumento[2]), &
						  Long(istr_Mant.Argumento[3])) = -1 OR &
		dw_3.Retrieve(Integer(istr_Mant.Argumento[1]), &
						  Integer(istr_Mant.Argumento[7]), &
						  Long(istr_Mant.Argumento[6])) =  -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
	 	DO
//			dw_6.DataObject	=	"dw_mant_movtoenvaenca_comercial" //cliente
//			dw_7.DataObject	=	"dw_mues_movtoenvadeta"
			IF dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  Long(istr_Mant.Argumento[3]), iuo_cliente.Codigo) = -1 OR &
				dw_5.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  Long(istr_Mant.Argumento[3]),1, iuo_cliente.Codigo) = -1 OR &
				dw_6.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[2]), &
								  Long(istr_Mant.Argumento[3])) = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE
				IF dw_5.RowCount() > 0 THEN dw_7.Retrieve(dw_5.object.enva_tipoen[dw_5.rowCount()], dw_5.object.enva_codigo[dw_5.rowCount()])
				
				ll_Fila_d = dw_4.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  Integer(istr_Mant.Argumento[7]), &
								  Long(istr_Mant.Argumento[6])) 
			  
				tab_1.tp_1.Enabled 	=	True
				tab_1.tp_2.Enabled 	=	True
				
				pb_eliminar.Enabled	=	True
				pb_grabar.Enabled		=	True
				pb_imprimir.Enabled	=	True
				pb_ins_det.Enabled	=	True

				HabilitaEncab(False)
				
				pb_eli_det.Enabled	=	True
				
				IF dw_4.RowCount() > 0 THEN
					li_tipodoc	=	dw_2.Object.mfco_tipdoc[1]
					ll_docrel	=	dw_2.Object.mfco_docrel[1]

					dw_4.SetFilter("")
					dw_4.Filter()
					
					dw_4.SetFilter("lfcd_tipdoc = " + String(li_tipodoc) + " And lfcd_docrel = " + String(ll_docrel))
					dw_4.Filter()
				END IF
				
				il_fila = 1
				dw_4.SetRow(1)
				dw_4.SetFocus()
				
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
	dw_3.SetRedraw(True)
	dw_4.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo;Long		ll_modif1, ll_modif2

ib_ok	= True

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE -1
			ib_ok = False
		CASE 0
			ll_modif1	=	dw_4.GetNextModified(0, Primary!)
			ll_modif1	+=	dw_2.GetNextModified(0, Primary!)
 		   ll_modif1	+=	dw_6.GetNextModified(0, Primary!)

			IF dw_4.RowCount() > 0 AND ll_modif1 > 0 THEN
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

dw_4.SetFilter("")
dw_4.Filter()

dw_1.Reset()
dw_5.Reset()
dw_6.Reset()
dw_7.Reset()
pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_imprimir.Enabled		=	False

dw_2.Enabled				=	True
dw_3.Enabled				=	True

dw_2.SetRedraw(False) 
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetRedraw(True)

dw_3.SetRedraw(False)
dw_3.Reset()
dw_3.InsertRow(0)
dw_3.SetRedraw(True)

dw_4.SetRedraw(False)
dw_4.Reset()
dw_4.SetRedraw(TRUE)

dw_2.SetFocus()
HabilitaEncab(True)

iuo_cliente.existe(gi_CodExport,True,Sqlca)
dw_2.SetItem(1, "clie_codigo", iuo_cliente.Codigo)

dw_2.SetItem(1, "plde_codigo", Integer(istr_Mant.Argumento[1]))
dw_2.SetItem(1, "tpmv_codigo", Integer(istr_Mant.Argumento[2]))
dw_2.SetItem(1, "mfco_fecmov", Today())

dw_3.SetItem(1, "lofc_pltcod", Integer(istr_Mant.Argumento[1]))

istr_mant.Argumento[10] = "0"
istr_mant.Argumento[3] 	= "0"
istr_mant.Argumento[7] 	= ""
istr_mant.Argumento[6] 	= ""
istr_mant.Argumento[16] = String(gi_CodExport)
end event

event ue_nuevo_detalle;datawindowchild ldwc_envases, ldwc_categoria

istr_mant.Borra	=	False
istr_mant.Agrega	=	True

habilitaencab( False)

IF tab_1.SelectedTab = 1 THEN
	dw_4.SetColumn("lfcd_nturno")
	
	il_fila = dw_4.InsertRow(0)
	dw_4.Object.lfcd_bultos[il_fila] = 1
	
	IF il_fila > 0 THEN
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
		Habilitaencab(FALSE)
	END IF
	
	IF dw_4.RowCount() > 0 THEN
		pb_eli_det.Enabled	= True
	END IF
	
	dw_4.GetChild("enva_tipoen", ldwc_envases)
	dw_4.GetChild("cate_codigo", ldwc_categoria)
	ldwc_envases.SetTransObject(SQLCA)
	ldwc_categoria.SetTransObject(SQLCA)
	if ldwc_envases.Retrieve() < 1 then ldwc_envases.InsertRow(0)
	if ldwc_categoria.Retrieve() < 1 then ldwc_categoria.InsertRow(0)
	
	dw_4.ScrollToRow(il_fila)
	dw_4.SetRow(il_fila)
	dw_4.SetFocus()
	dw_4.SetColumn(1)
	
ELSE
	istr_mant.dw	=	dw_5
	istr_mant.argumento[15] = istr_mant.argumento[7]
	istr_mant.argumento[7]  = "0"
	istr_mant.argumento[16]  = string(iuo_cliente.Codigo)
	
	OpenWithParm(iw_mantencion_2, istr_mant)

   istr_mant.argumento[7] = istr_mant.argumento[15]
	
	IF dw_5.RowCount() > 0 THEN
		pb_eli_det.Enabled			=	True
	ELSE
		pb_eli_det.Enabled			=	False
	END IF
	
	dw_5.SetRow(il_Fila)
	dw_5.SelectRow(il_Fila, True)
	
END IF

IF dw_4.RowCount() > 0 AND dw_5.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_4.RowCount() > 0 AND dw_5.RowCount() > 0 AND Not pb_eliminar.Enabled THEN
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
END IF
end event

event ue_borra_detalle();Integer li_planta, li_especie, li_secuencia, li_tipoen, li_envase
String  ls_calida
Long    ll_lote, ll_filaborra
SetPointer(HourGlass!)

ib_Borrar				=	True
Message.DoubleParm	=	0
istr_mant.Borra		=	True
istr_mant.Agrega		=	False

w_main.SetMicroHelp("Validando la eliminación de detalle...")

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

IF tab_1.SelectedTab = 1 THEN
	
	IF dw_4.rowcount() < 1 THEN RETURN
	
	IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
		
		IF dw_4.GetItemStatus(il_Fila, 0, Primary!) = NewModified! THEN
			IF dw_4.DeleteRow(0) = 1 THEN
				ib_borrar = False
				w_main.SetMicroHelp("Borrando Registro...")
				SetPointer(Arrow!)
			ELSE
				ib_borrar = False
				MessageBox(This.Title,"No se puede borrar actual registro.")
			END IF
	
			IF dw_4.RowCount() = 0 THEN
				pb_eliminar.Enabled = False
			ELSE
				il_fila = dw_4.GetRow()
			END IF
	   ELSE	
			li_planta  		=	dw_4.Object.lofc_pltcod[il_fila]
			li_especie 		=	dw_4.Object.lofc_espcod[il_fila]
			ll_lote    		=	dw_4.Object.lofc_lotefc[il_fila]
			li_secuencia	=	dw_4.Object.lfcd_secuen[il_fila]
			
			IF borradetallemovto(li_planta,li_especie,ll_lote,li_secuencia) THEN
				IF dw_4.DeleteRow(0) = 1 THEN
					ib_borrar = False
					w_main.SetMicroHelp("Borrando Registro...")
					SetPointer(Arrow!)
				ELSE
					ib_borrar = False
					MessageBox(This.Title,"No se puede borrar actual registro.")
				END IF
	
				IF dw_4.RowCount() = 0 THEN
					pb_eliminar.Enabled = False
				ELSE
					il_fila = dw_4.GetRow()
				END IF
			ELSE
					ib_borrar = False
					MessageBox(This.Title,"No se puede borrar registro Actual.")
			END IF
		END IF	
	END IF
	
ELSE
	
	istr_Mant.dw	=	dw_5
	
	li_tipoen = dw_5.Object.enva_tipoen[il_fila]
	li_envase = dw_5.Object.enva_codigo[il_fila]
	ls_calida = dw_5.Object.cale_calida[il_fila]
	
	OpenWithParm(iw_mantencion_2, istr_Mant)
	
	istr_Mant	=	Message.PowerObjectParm
	
	IF istr_mant.Respuesta = 1 THEN
		ll_filaborra = dw_7.Find("enva_tipoen = " + String(li_tipoen) + " AND " + &
		                         "enva_codigo = " + String(li_envase) + " AND " + &
										 "cale_calida = '" + ls_calida + "'",1,dw_7.RowCount())
		 
		IF ll_filaborra > 0 THEN
			dw_7.DeleteRow(ll_filaborra)
		END IF
		
		IF dw_5.DeleteRow(0) = 1 THEN
			ib_Borrar	=	False
			
			w_main.SetMicroHelp("Borrando Registro...")
			SetPointer(Arrow!)
		ELSE
			ib_Borrar	=	False
			
			MessageBox(This.Title,"No se puede borrar actual registro.")
		END IF
	
		IF dw_5.RowCount() = 0 THEN 
			HabilitaEncab(True)
			pb_eli_det.Enabled	=	False
		END IF
	END IF
END IF

istr_mant.Borra	 = False
SetPointer(Arrow!)


end event

event ue_antesguardar;Long		ll_Fila, ll_Nuevo, ll_FilaMov, ll_Numerolote,ll_Filaenv, ll_docrel
Integer	li_Exportador, li_Planta, li_TipoMovto, li_Secuencia, li_especie, &
         li_tipomovtoenva, li_tipodoc 
Boolean	lb_Existe = False, lb_Actualiza_Fruta = False, lb_Actualiza_Envase = False

ib_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

li_Planta			=	dw_2.Object.plde_codigo[1]
li_TipoMovto		=	Integer(istr_Mant.Argumento[2])
li_TipoMovtoEnva 	=	43
li_especie			=	Integer(istr_Mant.Argumento[7])
ll_Fila = 1

dw_4.SetRedraw(False)

Message.DoubleParm = 0

TriggerEvent("ue_validaregistro")

IF Message.DoubleParm = -1 THEN RETURN

IF Istr_Mant.Argumento[11] = "2" THEN
	dw_3.Object.prod_codigo[1] = dw_2.Object.prod_codigo[1]
	dw_3.Object.prod_nombre[1] = dw_2.Object.prod_nombre[1]
ELSEIF Istr_Mant.Argumento[11] = "1" THEN
	IF iuo_productores.Existe(gstr_paramplanta.productorempresa,True,SqlCa) THEN
		dw_3.SetItem(1, "prod_codigo", gstr_paramplanta.productorempresa )
		dw_3.SetItem(1, "prod_nombre", iuo_productores.Nombre)
	END IF	
END IF 	

DO WHILE ll_Fila <= dw_4.RowCount()
	IF dw_4.GetItemStatus(ll_Fila, 0, Primary!) = New! THEN
		dw_4.DeleteRow(ll_Fila)
	ELSE
		ll_Fila ++
	END IF
LOOP
 
IF NOT revisaenvases() THEN
	Message.DoubleParm = -1
	dw_4.SetRedraw(TRUE)
	RETURN
END IF	

li_tipodoc	=	dw_2.Object.mfco_tipdoc[1]
ll_docrel	=	dw_2.Object.mfco_docrel[1]

dw_4.SetFilter("")
dw_4.Filter()

IF dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN

	IF il_NumEnva=0 THEN
	
		iuo_TipoMovtoEnva.bloqueacorrel()	
		il_NumEnva = iuo_TipoMovtoEnva.UltimoCorrelativo(4,li_TipoMovtoEnva,li_Planta) 

		IF il_NumEnva = 0 OR IsNull(il_NumEnva) THEN
		  Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
		  Message.DoubleParm = -1
		  dw_4.SetRedraw(TRUE)
		  RETURN
		ELSE
		  lb_Actualiza_Envase = TRUE
		END IF
	 END IF

	IF il_NumFruta=0 THEN
	  iuo_TipoMovtoFruta.bloqueacorrel()
	  il_NumFruta = iuo_TipoMovtoFruta.UltimoCorrelativo(2,li_TipoMovto,li_Planta) 

	  IF il_NumFruta = 0 OR IsNull(il_NumFruta) THEN
		 Messagebox("Error de Conección","Vuelva a Intentar Grabar.")
		 Message.DoubleParm = -1
		 dw_4.SetRedraw(TRUE)
		 RETURN
	  ELSE
		 lb_Actualiza_Fruta = TRUE	
	  END IF
	END IF	

	dw_2.Object.mfco_numero[1]			=	il_NumFruta
	dw_2.Object.mfco_estmov[1] 			=  1
	
	istr_Mant.Argumento[3] 					= 	String(il_NumFruta)
	
	ll_Fila	=	dw_6.InsertRow(0)

	dw_6.Object.plde_codigo[ll_Fila]		=	dw_2.Object.plde_codigo[1]
	dw_6.Object.tpmv_codigo[ll_Fila]		=	li_TipoMovtoEnva
	dw_6.Object.meen_numero[ll_Fila]	=  il_NumEnva

   	dw_6.Object.prod_codigo[ll_Fila]		=	dw_2.Object.prod_codigo[1]
	dw_6.Object.meen_fecmov[ll_Fila]	=	dw_2.Object.mfco_fecmov[1]
	//dw_6.Object.tran_codigo[ll_Fila]		=	0  Ahora acepta Nulo
	dw_6.Object.meen_modulo[ll_Fila]	=	2
	dw_6.Object.tpmv_codrec[ll_fila] 		=  li_TipoMovto
	dw_6.Object.mfge_numero[ll_fila] 	=  il_NumFruta
	
	//Preguntar el Momento de Actualización
	IF lb_Actualiza_Fruta  THEN iuo_TipoMovtoFruta.Actualiza_Correlativo(2,li_Planta,li_TipoMovto,il_NumFruta) 
	IF lb_Actualiza_Envase THEN iuo_TipoMovtoEnva.Actualiza_Correlativo(4,li_Planta,li_TipoMovtoEnva,il_NumEnva) 
	///////////////////////////////////////
	
ELSE
	
	il_NumFruta						=	dw_2.Object.mfco_numero[1]
	istr_Mant.Argumento[3] 		= 	String(il_NumFruta)
	
END IF

IF IsNull(dw_3.Object.lofc_lotefc[1]) OR dw_3.Object.lofc_lotefc[1] = 0 THEN
	ll_numerolote = Nuevolote(ll_numerolote) + TipoRecepcion(istr_Mant.Argumento[11], &
						 Date(Mid(istr_Mant.Argumento[12],1,10)))
	dw_3.Object.lofc_lotefc[1] = ll_numerolote
END IF

ll_numerolote = dw_3.Object.lofc_lotefc[1]

dw_3.Object.lofc_totbul[1]	=	dw_4.Object.total_bulto[1]
dw_3.Object.lofc_totkil[1]	=	dw_4.Object.total_kilos[1]
dw_3.Object.lofc_tipool[1] 	=  Integer(Istr_Mant.Argumento[11])

SELECT	IsNull(Max(lfcd_secuen), 0) + 1
	INTO	:li_Secuencia
	FROM	dba.spro_lotesfrutacomdeta
	WHERE	lofc_pltcod	=	:li_Planta
	AND	lofc_espcod	=	:li_especie
	AND   lofc_lotefc =  :ll_Numerolote;

FOR ll_Fila = 1 TO dw_4.RowCount()
	IF dw_4.GetItemStatus(ll_Fila, 0, Primary!) 	= NewModified! THEN
		dw_4.Object.lofc_pltcod[ll_Fila]			=	dw_3.Object.lofc_pltcod[1]
		dw_4.Object.lofc_espcod[ll_Fila]			=	dw_3.Object.lofc_espcod[1]
		dw_4.Object.lofc_lotefc[ll_Fila]				=	dw_3.Object.lofc_lotefc[1]
		dw_4.Object.lfcd_secuen[ll_Fila]			=	li_Secuencia
		dw_4.Object.lfcd_fecham[ll_fila] 			=  dw_2.Object.mfco_fecmov[1]
		dw_4.Object.prod_codigo[ll_Fila]			=	dw_2.Object.prod_codigo[1]
		dw_4.Object.vari_codigo[ll_Fila]			=	dw_2.Object.vari_codigo[1]
		dw_4.Object.frio_tipofr[ll_Fila]				=	dw_2.Object.frio_tipofr[1]
		dw_4.Object.pefr_codigo[ll_Fila]			=	dw_2.Object.pefr_codigo[1]
		dw_4.Object.lfcd_tipdoc[ll_Fila]			=	dw_2.Object.mfco_tipdoc[1]
		dw_4.Object.lfcd_docrel[ll_Fila]			=	dw_2.Object.mfco_docrel[1]
		dw_4.Object.lfcd_tipool[ll_Fila] 			=  Integer(Istr_Mant.Argumento[11])
		
		li_Secuencia ++
	END IF
NEXT

 SELECT	IsNull(Max(mfcd_secuen), 0) + 1
	INTO	:li_Secuencia
	FROM	dba.spro_movtofrutacomdeta
	WHERE	plde_codigo	=	:li_Planta
	AND	tpmv_codigo	=	:li_TipoMovto
	AND   mfco_numero =  :il_NumFruta;

FOR ll_Fila = 1 TO dw_4.RowCount()
	IF dw_4.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		ll_Nuevo	=	dw_1.InsertRow(0)

		dw_1.Object.plde_codigo[ll_Nuevo]		=	dw_2.Object.plde_codigo[1]
		dw_1.Object.tpmv_codigo[ll_Nuevo]		=	dw_2.Object.tpmv_codigo[1]
		dw_1.Object.mfco_numero[ll_Nuevo]		=	dw_2.Object.mfco_numero[1]
		dw_1.Object.mfcd_secuen[ll_Nuevo]		=	li_Secuencia
		dw_1.Object.plde_coorde[ll_Nuevo]		=	dw_2.Object.plde_codigo[1]
		dw_1.Object.cama_codigo[ll_Nuevo]		=	1
		dw_1.Object.lofc_pltcod[ll_Nuevo]			=	dw_2.Object.plde_codigo[1]
		dw_1.Object.lofc_espcod[ll_Nuevo]		=	dw_3.Object.lofc_espcod[1]
		dw_1.Object.lofc_lotefc[ll_Nuevo]			=	dw_4.Object.lofc_lotefc[1]
		dw_1.Object.lfcd_secuen[ll_Nuevo]		=	dw_4.Object.lfcd_secuen[ll_Fila]
		dw_1.Object.mfcd_bulent[ll_Nuevo]		=	dw_4.Object.lfcd_bultos[ll_Fila]
		dw_1.Object.mfcd_kgnent[ll_Nuevo]		=	dw_4.Object.lfcd_kilnet[ll_Fila]
		dw_1.Object.vari_codigo[ll_Nuevo]		=	dw_4.Object.vari_codigo[ll_Fila]
		dw_1.Object.mfcd_calibr[ll_Nuevo]		=	dw_4.Object.refe_gcalib[ll_Fila]
		
		li_Secuencia ++
	ELSEIF dw_4.GetItemStatus(ll_Fila, 0, Primary!) = DataModified! THEN

		ll_FilaMov	=	dw_1.Find("lofc_lotefc = " + String(dw_4.Object.lofc_lotefc[ll_Fila]) + &
										 " AND lfcd_secuen = " + String(dw_4.Object.lfcd_secuen[ll_Fila]), + &
										 1, dw_1.RowCount())

		dw_1.Object.mfcd_bulent[ll_FilaMov]	=	dw_4.Object.lfcd_bultos[ll_Fila]
		dw_1.Object.mfcd_kgnent[ll_FilaMov]	=	dw_4.Object.lfcd_kilnet[ll_Fila]

	END IF
NEXT

FOR ll_Fila = 1 TO dw_5.RowCount()
	IF dw_5.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
		dw_5.Object.plde_codigo[ll_Fila]	=	dw_2.Object.plde_codigo[1]
		dw_5.Object.tpmv_codigo[ll_Fila]	=	li_TipoMovtoEnva
		dw_5.Object.meen_numero[ll_Fila]	=	dw_6.Object.meen_numero[1]
		dw_5.Object.fgme_sentid[ll_Fila]	=	1
	END IF
NEXT

dw_4.SetFilter("lfcd_tipdoc = " + String(li_tipodoc) + " And lfcd_docrel = " + String(ll_docrel))
dw_4.Filter()
dw_4.SetRedraw(TRUE)

//revisar asignacion
IF Istr_Mant.Argumento[11] = "2" THEN
	dw_3.Object.prod_codigo[1] = dw_2.Object.prod_codigo[1]
	dw_3.Object.prod_nombre[1] = dw_2.Object.prod_nombre[1]
ELSEIF Istr_Mant.Argumento[11] = "1" THEN
	IF iuo_productores.Existe(gstr_paramplanta.productorempresa,True,SqlCa) THEN
		dw_3.SetItem(1, "prod_codigo", gstr_paramplanta.productorempresa )
		dw_3.SetItem(1, "prod_nombre", iuo_productores.Nombre)
	END IF	
END IF 	

//
AsignaClienteA_dws()
//
end event

event ue_guardar();IF dw_1.AcceptText() = -1 THEN RETURN
IF dw_3.AcceptText() = -1 THEN RETURN
IF dw_4.AcceptText() = -1 THEN RETURN
IF dw_5.AcceptText() = -1 THEN RETURN

IF dw_4.RowCount() <= 0 OR dw_5.RowCount() <= 0 THEN Message.DoubleParm = -1
SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
	
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF

IF il_NumFruta>0 AND dw_2.GetItemStatus(1, 0, Primary!) = NotModified! THEN
	il_NumFruta = 0
	il_NumEnva	= 0
END IF
end event

event ue_seleccion;call super::ue_seleccion;Long				ll_lote
Str_Busqueda	lstr_Busq

lstr_Busq.Argum[1] = istr_Mant.Argumento[1]		// Código Planta
lstr_Busq.Argum[2] = istr_Mant.Argumento[2]		// Tipo de Movimiento
lstr_Busq.Argum[3] = ''									// Estado Movimiento
lstr_Busq.Argum[4] = ''  								// Fecha Inicio Movimiento
lstr_Busq.Argum[16] = string(iuo_cliente.Codigo)	// Fecha Inicio Movimiento
OpenWithParm(w_busc_movtofrutacomenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	istr_Mant.Argumento[1]	=	lstr_Busq.Argum[1]
	istr_Mant.Argumento[2]	=	lstr_Busq.Argum[2]
	istr_Mant.Argumento[3]	=	lstr_Busq.Argum[3]
	istr_Mant.Argumento[4]	=	lstr_Busq.Argum[10]
	istr_Mant.Argumento[5]	=	lstr_Busq.Argum[11]

	IF iuo_spro_ordenproceso.Existe(Integer(istr_Mant.Argumento[1]), &
									  	  Integer(istr_mant.Argumento[4]), &
									  	  Long(istr_Mant.Argumento[5]), True, SqlCa, iuo_cliente.Codigo) THEN

		istr_Mant.Argumento[7]	= String(iuo_spro_ordenproceso.Especie)
		iuo_spro_ordenproceso.CapturaReferencias(1,0,1,0,0,False,Sqlca)
		
		dw_2.SetItem(1, "mfco_docrel", Integer(istr_Mant.Argumento[5]))
		dw_2.SetItem(1, "prod_codigo", iuo_spro_ordenproceso.productor)
		dw_2.SetItem(1, "prod_nombre", iuo_spro_ordenproceso.NombreProductor)
		dw_2.SetItem(1, "frio_tipofr", iuo_spro_ordenproceso.TipoFrio)
		dw_2.SetItem(1, "pefr_codigo", iuo_spro_ordenproceso.PeriodoFrio)
		dw_2.SetItem(1, "espe_codigo", iuo_spro_ordenproceso.Especie)
		dw_2.SetItem(1, "vari_codigo", iuo_spro_ordenproceso.variedad)
		dw_2.SetItem(1, "vari_nombre", iuo_spro_ordenproceso.NombreVariedad)
		
		dw_3.SetItem(1, "lofc_espcod", iuo_spro_ordenproceso.Especie)
		iuo_prodempresa.existe(gstr_paramplanta.productorempresa,false,sqlca)
	
		dw_3.SetItem(1, "prod_codigo", gstr_paramplanta.productorempresa)
		dw_3.SetItem(1, "prod_nombre", iuo_prodempresa.nombre )

		dw_3.SetItem(1, "prod_codigo", iuo_spro_ordenproceso.Productor)
		dw_3.SetItem(1, "prod_nombre", iuo_spro_ordenproceso.NombreProductor)

		dw_3.Enabled = True
		HabilitaLote()

	END IF

	dw_2.SetItem(1, "mfco_docrel", Integer(istr_Mant.Argumento[5]))	
	IF iuo_spro_ordenproceso.Existe(Integer(istr_Mant.Argumento[1]), &
										  integer(istr_mant.Argumento[4]), &
										  Long(istr_Mant.Argumento[5]), True, SqlCa, iuo_cliente.Codigo) THEN
		
		istr_Mant.Argumento[7]	= String(iuo_spro_ordenproceso.Especie)
		istr_Mant.Argumento[12] = Mid(String(iuo_spro_ordenproceso.FechaOrden),1,10)				
		HabilitaLote()
		iuo_spro_ordenproceso.CapturaReferencias(1,0,1,0,0,False,Sqlca)

	END IF
	IF (istr_Mant.Argumento[11] = "1" AND gstr_paramplanta.PoolVenta <> 1 AND &
													 gstr_paramplanta.PoolVenta <> 5) OR &
		(istr_Mant.Argumento[11] = "2" AND gstr_Paramplanta.PoolRetiro <> 1 AND &
													 gstr_Paramplanta.PoolRetiro <> 4) THEN

		Istr_Mant.Argumento[6] = String(Nuevolote(ll_lote) + TipoRecepcion(istr_Mant.Argumento[11], &
												  Date(Mid(istr_Mant.Argumento[12],1,10))))
		dw_3.Object.lofc_lotefc[1] = Long(Istr_Mant.Argumento[6])
	ELSE
		ExisteLote(0)
	END IF
	
	IF istr_Mant.Argumento[6] <> "0" AND Existelote(Integer(Istr_Mant.Argumento[6])) THEN
		TriggerEvent("ue_recuperadatos")
	ELSEIF (Istr_Mant.Argumento[11] = "1") AND &
			 gstr_paramplanta.PoolVenta = 5 OR gstr_paramplanta.PoolVenta = 1 THEN 
			 dw_3.Object.lofc_lotefc[1] = Integer("")
			 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
	ELSEIF (Istr_Mant.Argumento[11] = "2") AND &
			 gstr_paramplanta.PoolRetiro = 1 OR gstr_paramplanta.PoolRetiro = 4 THEN 
			 dw_3.Object.lofc_lotefc[1] = Integer("")
			 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
	ELSE
			 dw_3.Object.lofc_lotefc[1] = Integer(Istr_Mant.Argumento[6])
			 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
			 Habilitaencab(False)
			 dw_5.Retrieve(Integer(istr_Mant.Argumento[1]), &
								Integer(istr_Mant.Argumento[2]), &
									Long(istr_Mant.Argumento[3]),1, iuo_cliente.Codigo)
			 
	END IF

	HabilitaLote()
END IF
HabilitaIngreso("mfco_docrel")
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila, ll_lote
Integer  li_plantalote, li_espelote

istr_info.titulo	= "IDENTIFICACION FRUTA COMERCIAL"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_identificacion_fruta_comercial"

vinf.dw_1.SetTransObject(sqlca)

li_plantalote = dw_3.Object.lofc_pltcod[1]
li_espelote   = dw_3.Object.lofc_espcod[1]
ll_lote       = dw_3.Object.lofc_lotefc[1]

fila = vinf.dw_1.Retrieve(integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]), &
                          Long(istr_mant.argumento[3]), li_plantalote, li_espelote, ll_lote, dw_2.Object.clie_codigo[1])

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN
SetPointer(HourGlass!)
ib_borrar		=	True
ib_AutoCommit	=	sqlca.AutoCommit
w_main.SetMicroHelp("Validando la eliminación...")
Message.DoubleParm = 0
This.TriggerEvent ("ue_validaborrar")
IF Message.DoubleParm = -1 THEN RETURN
IF dw_5.RowCount() > 0 THEN
	dw_5.RowsMove(1, dw_5.RowCount(), Primary!, dw_5, 1, Delete!)
END IF
IF dw_7.RowCount() > 0 THEN
	dw_7.RowsMove(1, dw_7.RowCount(), Primary!, dw_7, 1, Delete!)
END IF
IF dw_4.RowCount() > 0 THEN
	dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
END IF
IF dw_1.RowCount() > 0 THEN
	dw_1.RowsMove(1, dw_1.RowCount(), Primary!, dw_1, 1, Delete!)
END IF
IF dw_3.DeleteRow(0) = 1 AND dw_6.DeleteRow(0) = 1 AND dw_2.DeleteRow(0) = 1 THEN
		ib_Borrar	=	False
		w_main.SetMicroHelp("Borrando Registro...")
		IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
		ELSE
			w_main.SetMicroHelp("Registro no Borrado...")
		END IF			
ELSE
	ib_Borrar	=	False
	MessageBox(This.Title,"No se puede borrar actual registro.")
END IF
Long		ll_Fila, ll_NrLote, ll_Secuen, ll_Envases
Integer	li_Planta, li_Especie, li_Elimina

IF dw_2.RowCount() < 1 OR dw_4.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)
ib_borrar		=	True
ib_AutoCommit	=	sqlca.AutoCommit

w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN
dw_4.SetRedraw(False)

IF Not revisaenvases() THEN
	Message.DoubleParm = -1
	dw_4.SetRedraw(True)
	RETURN
END IF

dw_4.SetRedraw(True)

FOR ll_Fila = 1 TO dw_4.RowCount()
	li_Planta	=	dw_4.Object.lofc_pltcod[ll_Fila]
	li_Especie	=	dw_4.Object.lofc_espcod[ll_Fila]
	ll_NrLote	=	dw_4.Object.lofc_lotefc[ll_Fila]
	ll_Secuen	=	dw_4.Object.lfcd_secuen[ll_Fila]
	li_Elimina	=	dw_1.Find("lofc_pltcod = " + String(li_Planta) + &
						" AND lofc_espcod = " + String(li_Especie) + &
						" AND lofc_lotefc = " + String(ll_NrLote) + &
						" AND lfcd_secuen = " + String(ll_Secuen), 1, dw_1.RowCount())
	IF li_Elimina > 0 THEN dw_1.RowsMove(li_Elimina, li_Elimina, Primary!, dw_1, 1, Delete!)
NEXT

ll_Envases	=	Long(dw_4.Object.lfcd_bultos[1])
dw_4.RowsMove(1, dw_4.RowCount(), Primary!, dw_4, 1, Delete!)
dw_3.RowsMove(1, dw_3.RowCount(), Primary!, dw_3, 1, Delete!)

IF dw_1.RowCount() = 0 THEN
	dw_5.RowsMove(1, dw_5.RowCount(), Primary!, dw_5, 1, Delete!)
	dw_7.RowsMove(1, dw_7.RowCount(), Primary!, dw_7, 1, Delete!)
	dw_6.RowsMove(1, dw_6.RowCount(), Primary!, dw_6, 1, Delete!)
	dw_2.RowsMove(1, dw_2.RowCount(), Primary!, dw_2, 1, Delete!)
ELSE
	dw_5.Object.fgme_cantid[1]	=	Long(dw_5.Object.fgme_cantid[1]) - ll_Envases
END IF

ib_Borrar	=	False
w_main.SetMicroHelp("Borrando Registro...")

IF wf_actualiza_db(True) THEN
	w_main.SetMicroHelp("Registro Borrado...")
	IF ExisteLote(0) THEN
		This.TriggerEvent("ue_recuperadatos")
	ELSE
		This.TriggerEvent("ue_nuevo")
	END IF
ELSE
	w_main.SetMicroHelp("Registro no Borrado...")
	MessageBox(This.Title,"No se puede borrar Número de Lote.")
END IF
end event

event ue_modifica_detalle;istr_mant.agrega	=	False
istr_mant.borra	=	False

IF tab_1.SelectedTab = 2 THEN
	
	IF dw_5.RowCount() > 0 THEN
		istr_mant.dw		=	dw_5
		
		istr_mant.Argumento[16] = String(iuo_cliente.Codigo)
		istr_mant.agrega 			= FALSE
		OpenWithParm(iw_mantencion_2, istr_mant)
	END IF
END IF
end event

event timer;call super::timer;Integer	li_factor, li_posini, li_LarBuf
String 	ls_string
Double	ld_kilos

Ole_Puerta.Object.inputlen =	Integer(istr_puertacomm.LargoLectura)

li_LarBuf =Ole_Puerta.Object.InBufferCount

IF li_LarBuf > 0 THEN
	ls_string =  Ole_Puerta.Object.input
END IF

li_posini = Pos(ls_string,istr_puertacomm.CadenaInicio) + Len(istr_puertacomm.CadenaInicio)

IF Len(Mid(ls_string,li_posini)) < istr_puertacomm.LargoCadena THEN RETURN
	
IF IsNumber(Mid(ls_string,li_posini,istr_puertacomm.LargoCadena)) THEN
	ld_kilos	=	Dec(Mid(ls_string,li_posini,istr_puertacomm.LargoCadena))
	IF istr_puertacomm.Decimales > 0 THEN
		li_factor	= 10 ^ istr_puertacomm.Decimales
		ld_kilos		= Round(ld_kilos/li_factor,istr_puertacomm.Decimales)
	END IF
	em_kilos.Text	=	String(ld_kilos)
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_movtofrutacomer_granel
boolean visible = false
integer x = 142
integer y = 1308
integer width = 2761
integer height = 380
string title = "Detalle de Movimiento"
string dataobject = "dw_mues_movtofrutacomdeta"
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_movtofrutacomer_granel
integer x = 32
integer y = 32
integer width = 2889
integer height = 464
integer taborder = 10
string dataobject = "dw_mant_movtofrutacomenca_proceso"
end type

event dw_2::itemchanged;Integer  li_lote, ll_lote
String	ls_Columna, ls_Null, ls_Fecha
Boolean  lb_Exislote

SetNull(ls_Null)

ls_Columna = dwo.name

CHOOSE CASE ls_Columna
	CASE "clie_codigo"

		IF Not iuo_cliente.existe(integer(data),True,Sqlca) THEN
			This.SetItem(1, "clie_codigo", integer(ls_Null))
		ELSE	
			istr_Mant.Argumento[16]	= Data
			This.SetItem(1, "mfco_docrel", Long(ls_Null))
			This.SetItem(1, "prod_nombre", Integer(ls_Null))
			This.SetItem(1, "frio_tipofr", ls_Null)
			This.SetItem(1, "pefr_codigo", Integer(ls_Null))
			This.SetItem(1, "espe_codigo", Integer(ls_Null))
			This.SetItem(1, "vari_nombre", ls_Null)
		END IF
		
	CASE "plde_codigo"

		IF Not iuo_planta.existe(integer(data),True,Sqlca) THEN
			This.SetItem(1, "plde_codigo", integer(ls_Null))
		ELSE	
			istr_Mant.Argumento[1]	= Data
			This.SetItem(1, "mfco_docrel", Long(ls_Null))
			This.SetItem(1, "prod_nombre", Integer(ls_Null))
			This.SetItem(1, "frio_tipofr", ls_Null)
			This.SetItem(1, "pefr_codigo", Integer(ls_Null))
			This.SetItem(1, "espe_codigo", Integer(ls_Null))
			This.SetItem(1, "vari_nombre", ls_Null)
		END IF
		       
	CASE "mfco_tipdoc" 
		istr_Mant.Argumento[4]	= Data
		This.SetItem(1, "mfco_docrel", Long(ls_Null))
		This.SetItem(1, "prod_nombre", Integer(ls_Null))
		This.SetItem(1, "frio_tipofr", ls_Null)
		This.SetItem(1, "pefr_codigo", Integer(ls_Null))
		This.SetItem(1, "espe_codigo", Integer(ls_Null))
		This.SetItem(1, "vari_nombre", ls_Null)

	CASE "mfco_docrel"
		IF iuo_spro_ordenproceso.Existe(Integer(istr_Mant.Argumento[1]), &
												  This.Object.mfco_tipdoc[1], &
												  Long(Data), True, SqlCa, iuo_cliente.Codigo)       THEN
			istr_Mant.Argumento[5]	= Data
			istr_Mant.Argumento[7]	= String(iuo_spro_ordenproceso.Especie)
			Habilitalote()
			
			iuo_spro_ordenproceso.CapturaReferencias(1,0,1,0,0,False,Sqlca)

			This.SetItem(1, "prod_codigo", iuo_spro_ordenproceso.productor)
			This.SetItem(1, "prod_nombre", iuo_spro_ordenproceso.NombreProductor)
			This.SetItem(1, "frio_tipofr", iuo_spro_ordenproceso.TipoFrio)
			This.SetItem(1, "pefr_codigo", iuo_spro_ordenproceso.PeriodoFrio)
			This.SetItem(1, "espe_codigo", iuo_spro_ordenproceso.Especie)
			This.SetItem(1, "vari_codigo", iuo_spro_ordenproceso.variedad)
			This.SetItem(1, "vari_nombre", iuo_spro_ordenproceso.NombreVariedad)
			
			dw_3.SetItem(1, "lofc_espcod", iuo_spro_ordenproceso.Especie)
			iuo_prodempresa.existe(gstr_paramplanta.productorempresa,false,sqlca)
		
			dw_3.SetItem(1, "prod_codigo", iuo_spro_ordenproceso.Productor)
			dw_3.SetItem(1, "prod_nombre", iuo_spro_ordenproceso.NombreProductor)

			dw_3.Enabled = True
			
			IF buscamovto(Integer(istr_Mant.Argumento[4]),Long(data)) THEN
				IF (istr_Mant.Argumento[11] = "1" AND gstr_paramplanta.PoolVenta <> 1 AND &
				                       					 gstr_paramplanta.PoolVenta <> 5) OR &
				   (istr_Mant.Argumento[11] = "2" AND gstr_Paramplanta.PoolRetiro <> 1 AND &
																 gstr_Paramplanta.PoolRetiro <> 4) THEN
					Istr_Mant.Argumento[6] = String(Nuevolote(ll_lote) + TipoRecepcion(istr_Mant.Argumento[11], &
													 Date(Mid(String(iuo_spro_ordenproceso.FechaOrden),1,10))))
					dw_3.Object.lofc_lotefc[1] = nuevofoliolote()
				ELSE
					ExisteLote(0)
				END IF

				IF Existelote(Integer(Istr_Mant.Argumento[6])) AND istr_Mant.Argumento[6] <> "0" THEN
					Parent.TriggerEvent("ue_recuperadatos")
				ELSEIF (Istr_Mant.Argumento[11] = "1") AND &
						 gstr_paramplanta.PoolVenta = 5 OR gstr_paramplanta.PoolVenta = 1 THEN 
						 dw_3.Object.lofc_lotefc[1] = Integer("")
						 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
				ELSEIF (Istr_Mant.Argumento[11] = "2") AND &
						 gstr_paramplanta.PoolRetiro = 1 OR gstr_paramplanta.PoolRetiro = 4 THEN 

						 dw_3.Object.lofc_lotefc[1] = Integer("")
						 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
				ELSE
						 dw_3.Object.lofc_lotefc[1] = Integer(Istr_Mant.Argumento[6])
						 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
						 Habilitaencab(False)
						 dw_5.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  			Integer(istr_Mant.Argumento[2]), &
								  			   Long(istr_Mant.Argumento[3]),1,iuo_cliente.Codigo)
						 
				END IF
			ELSE
				IF (istr_Mant.Argumento[11] = "1" AND gstr_paramplanta.PoolVenta <> 1 AND &
				                       					 gstr_paramplanta.PoolVenta <> 5) OR &
				   (istr_Mant.Argumento[11] = "2" AND gstr_Paramplanta.PoolRetiro <> 1 AND &
																 gstr_Paramplanta.PoolRetiro <> 4) THEN
					Istr_Mant.Argumento[6] = String(Nuevolote(ll_lote) + TipoRecepcion(istr_Mant.Argumento[11], &
													 Date(Mid(String(iuo_spro_ordenproceso.FechaOrden),1,10))))
					IF dw_3.Retrieve(Integer(Istr_Mant.Argumento[1]), Integer(Istr_Mant.Argumento[7]), &
					                 Integer(Istr_Mant.Argumento[6])) <= 0 THEN
						dw_3.InsertRow(0)
						dw_3.SetItem(1, "lofc_pltcod", Integer(Istr_Mant.Argumento[1]))						
						dw_3.SetItem(1, "lofc_espcod", iuo_spro_ordenproceso.Especie)			
						dw_3.SetItem(1, "prod_codigo", iuo_spro_ordenproceso.Productor)
						dw_3.SetItem(1, "prod_nombre", iuo_spro_ordenproceso.NombreProductor)
//						dw_3.SetItem(1, "prod_codigo", gstr_paramplanta.productorempresa)
//						dw_3.SetItem(1, "prod_nombre", iuo_prodempresa.nombre )
						dw_3.Object.lofc_lotefc[1] = Long(Istr_Mant.Argumento[6])
					END IF
						
				ELSE
					ExisteLote(0)
				END IF

				IF (Istr_Mant.Argumento[11] = "1") AND &
						 gstr_paramplanta.PoolVenta = 5 OR gstr_paramplanta.PoolVenta = 1 THEN 
						 dw_3.Object.lofc_lotefc[1] = NuevoFolioLote() //Integer("")
						 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
				ELSEIF (Istr_Mant.Argumento[11] = "2") AND &
						 gstr_paramplanta.PoolRetiro = 1 OR gstr_paramplanta.PoolRetiro = 4 THEN 
						 dw_3.Object.lofc_lotefc[1] = Integer("")
						 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
				ELSE
						 dw_3.Object.lofc_lotefc[1] = Integer(Istr_Mant.Argumento[6])
						 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
						 Habilitaencab(False)
						 dw_5.Retrieve(Integer(istr_Mant.Argumento[1]), &
								  			Integer(istr_Mant.Argumento[2]), &
								  			   Long(istr_Mant.Argumento[3]),1, iuo_cliente.Codigo)
						 
				END IF
				
			END IF	
		ELSE
			This.SetItem(1, "prod_codigo", long(ls_Null))
			This.SetItem(1, "prod_nombre", ls_Null)
			This.SetItem(1, "frio_tipofr", ls_Null)
			This.SetItem(1, "pefr_codigo", Integer(ls_Null))
			This.SetItem(1, "espe_codigo", Integer(ls_Null))
			This.SetItem(1, "vari_codigo", integer(ls_Null))
			This.SetItem(1, "vari_nombre", ls_Null)
			This.SetItem(1, ls_Columna, Long(ls_Null))
			dw_3.Enabled = False
			RETURN 1
		END IF

	CASE "mfco_numero"
		
		IF ExisteMovimiento(Long(Data)) THEN
			istr_Mant.Argumento[3]	=	Data
			This.SetItem(1, "mfco_docrel", Integer(istr_Mant.Argumento[5]))	
			IF iuo_spro_ordenproceso.Existe(Integer(istr_Mant.Argumento[1]), &
												  integer(istr_mant.Argumento[4]), &
												  Long(istr_Mant.Argumento[5]), True, SqlCa, iuo_cliente.Codigo)       THEN
				
				istr_Mant.Argumento[7]	= String(iuo_spro_ordenproceso.Especie)
				HabilitaLote()
				iuo_spro_ordenproceso.CapturaReferencias(1,0,1,0,0,False,Sqlca)
	
				This.SetItem(1, "prod_codigo", iuo_spro_ordenproceso.productor)
				This.SetItem(1, "prod_nombre", iuo_spro_ordenproceso.NombreProductor)
				This.SetItem(1, "frio_tipofr", iuo_spro_ordenproceso.TipoFrio)
				This.SetItem(1, "pefr_codigo", iuo_spro_ordenproceso.PeriodoFrio)
				This.SetItem(1, "espe_codigo", iuo_spro_ordenproceso.Especie)
				This.SetItem(1, "vari_codigo", iuo_spro_ordenproceso.variedad)
				This.SetItem(1, "vari_nombre", iuo_spro_ordenproceso.NombreVariedad)
			
			END IF
			IF (istr_Mant.Argumento[11] = "1" AND gstr_paramplanta.PoolVenta <> 5) OR &
				(istr_Mant.Argumento[11] = "2" AND gstr_Paramplanta.PoolRetiro <> 4) THEN
			
				Istr_Mant.Argumento[6] = String(Nuevolote(ll_lote) + TipoRecepcion(istr_Mant.Argumento[11], &
												 Date(Mid(String(iuo_spro_ordenproceso.FechaOrden),1,10))))
				dw_3.Object.lofc_lotefc[1] = Long(Istr_Mant.Argumento[6])
			ELSE
				ExisteLote(0)
			END IF
			
			IF istr_Mant.Argumento[6] <> "0" AND Existelote(Integer(Istr_Mant.Argumento[6])) THEN
				Parent.TriggerEvent("ue_recuperadatos")
			ELSEIF gstr_paramplanta.PoolVenta = 5 OR gstr_paramplanta.PoolVenta = 1 THEN 
					 dw_3.Object.lofc_lotefc[1] = Integer("")
					 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
				 ELSE
					 dw_3.Object.lofc_lotefc[1] = Integer(Istr_Mant.Argumento[6])
					 dw_2.Object.mfco_numero[1] = Integer(Istr_Mant.Argumento[3])
					 Habilitaencab(False)
			END IF

		ELSE
			This.SetItem(1, ls_Columna, Long(ls_Null))
			RETURN 1
		END IF

	CASE "mfco_fecmov"
		ls_Fecha	=	Data
		This.SetItem(1, ls_Columna, Date(Mid(ls_Fecha,1,10)))

END CHOOSE

HabilitaIngreso(ls_columna)
end event

event dw_2::itemerror;call super::itemerror;RETURN 1
end event

event dw_2::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.name

	CASE "b_ordenproceso"
		BuscaOrdenProceso()

END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_movtofrutacomer_granel
integer x = 3273
integer y = 268
integer taborder = 50
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_movtofrutacomer_granel
integer x = 3273
integer taborder = 60
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_movtofrutacomer_granel
integer x = 3273
integer y = 632
integer taborder = 70
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_movtofrutacomer_granel
integer x = 3273
integer y = 808
integer taborder = 80
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_movtofrutacomer_granel
integer x = 3273
integer y = 988
integer taborder = 90
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_movtofrutacomer_granel
integer x = 3273
integer y = 1440
integer taborder = 100
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_movtofrutacomer_granel
integer x = 3273
integer y = 1612
integer taborder = 110
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_movtofrutacomer_granel
integer x = 3273
integer taborder = 40
end type

type dw_3 from uo_dw within w_maed_movtofrutacomer_granel
integer x = 32
integer y = 472
integer width = 2889
integer height = 288
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
string dataobject = "dw_mant_lotesfrutacomenc_proceso"
boolean vscrollbar = false
end type

event itemchanged;String	ls_Columna, ls_Null, ls_Fecha

SetNull(ls_Null)

ls_Columna = dwo.name
 
CHOOSE CASE ls_Columna
	CASE "lofc_lotefc"
		dw_4.reset()
		//dw_5.reset()		
		dw_3.reset()		
		IF Existelote(long(data)) THEN
         istr_mant.argumento[6] = Data
			Parent.TriggerEvent("ue_recuperadatos")
		ELSEIF istr_Mant.Argumento[11] = "1" AND gstr_paramplanta.PoolVenta <> 1 AND &
															gstr_paramplanta.PoolVenta <> 5 THEN
			dw_3.SetItem(1,"lofc_lotefc",long(ls_null))
			Return 1
		ELSEIF istr_Mant.Argumento[11] = "2" AND gstr_paramplanta.PoolVenta <> 1 AND &
															gstr_paramplanta.PoolVenta <> 4 THEN
			dw_3.SetItem(1,"lofc_lotefc",long(ls_null))
//			dw_3.SetItem(1, "prod_codigo", iuo_spro_ordenproceso.Productor)
//			dw_3.SetItem(1, "prod_nombre", iuo_spro_ordenproceso.NombreProductor)
			Return 1
		ELSEIF Loteconmov(Long(Data)) THEN
				dw_3.SetItem(1,"lofc_lotefc",long(ls_null))			
				RETURN 1
		ELSE
			dw_3.InsertRow(0)
			dw_3.object.lofc_lotefc[1] =	Integer(Data)
			dw_3.Object.lofc_pltcod[1]	=	Integer(istr_Mant.Argumento[1])
			dw_3.Object.lofc_espcod[1]	=	Integer(istr_Mant.Argumento[7])
			dw_3.Object.lofc_totbul[1] =	0
			dw_3.Object.lofc_totkil[1]	=	0
			dw_3.SetItem(1, "prod_codigo", iuo_spro_ordenproceso.Productor)
			dw_3.SetItem(1, "prod_nombre", iuo_spro_ordenproceso.NombreProductor)			
		END IF	

	CASE "prod_codigo"
		IF NOT iuo_productores.Existe(long(Data),True,SqlCa) THEN
			This.SetItem(1, ls_Columna, Integer(ls_Null))
			This.SetItem(1, "prod_nombre", ls_Null)
			RETURN 1
		ELSE
			This.SetItem(1, "prod_nombre", iuo_productores.Nombre)
		END IF

END CHOOSE
			
HabilitaIngreso(ls_columna)
end event

event buttonclicked;long ll_lote

CHOOSE CASE dwo.name

	CASE "b_nuevolote"
		dw_3.Reset()
		dw_3.insertrow(0)
		ll_lote = dw_3.Object.lofc_lotefc[1]		
   	ll_lote = Nuevolote(ll_lote) + TipoRecepcion(istr_Mant.Argumento[11], &
					 Date(istr_Mant.Argumento[12]))
      dw_3.Object.lofc_lotefc[1] = ll_lote
		dw_4.Reset()
		 
		dw_3.SetColumn("prod_codigo")
		dw_3.SetFocus()

		HabilitaIngreso("prod_codigo")
	CASE "b_prodrot"
		BuscaProductor()

END CHOOSE
end event

event itemerror;call super::itemerror;RETURN 1
end event

type tab_1 from tab within w_maed_movtofrutacomer_granel
event create ( )
event destroy ( )
integer x = 18
integer y = 800
integer width = 3136
integer height = 992
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 30586022
boolean fixedwidth = true
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tp_1 tp_1
tp_2 tp_2
end type

on tab_1.create
this.tp_1=create tp_1
this.tp_2=create tp_2
this.Control[]={this.tp_1,&
this.tp_2}
end on

on tab_1.destroy
destroy(this.tp_1)
destroy(this.tp_2)
end on

event selectionchanged;IF NewIndex = 1 THEN
	IF dw_4.RowCount() > 0 THEN
		pb_eli_det.Enabled	=	True
		il_Fila 					=	1
		
		dw_4.SelectRow(0,False)
		dw_5.SelectRow(0,False)
		dw_4.SetRow(il_Fila)
	ELSE
		pb_eli_det.Enabled	=	False
	END IF
ELSE
	IF dw_5.RowCount() > 0 THEN
		pb_eli_det.Enabled	=	True
		il_Fila 					=	1
		
		dw_4.SelectRow(0,False)
		dw_5.SelectRow(0,False)
		dw_5.SetRow(il_Fila)
		dw_5.SelectRow(il_Fila, True)
	ELSE
		pb_eli_det.Enabled	=	False
	END IF
END IF
end event

type tp_1 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 3099
integer height = 864
boolean enabled = false
long backcolor = 30586022
string text = "Detalle de Fruta"
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "Table!"
long picturemaskcolor = 553648127
dw_9 dw_9
dw_detalle dw_detalle
end type

on tp_1.create
this.dw_9=create dw_9
this.dw_detalle=create dw_detalle
this.Control[]={this.dw_9,&
this.dw_detalle}
end on

on tp_1.destroy
destroy(this.dw_9)
destroy(this.dw_detalle)
end on

type dw_9 from datawindow within tp_1
boolean visible = false
integer x = 681
integer y = 256
integer width = 1842
integer height = 400
integer taborder = 20
string title = "none"
string dataobject = "dw_pesaje_romana"
boolean hscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_detalle from uo_dw within tp_1
integer x = 32
integer y = 36
integer width = 3040
integer height = 796
integer taborder = 21
string dataobject = "dw_mues_lotesfrutacomdeta_proceso_bp"
boolean hscrollbar = true
boolean border = false
boolean hsplitscroll = true
end type

event clicked;IF Row > 0 THEN
	il_fila = Row
END IF

RETURN 0
end event

event doubleclicked;//w_maed_movtofrutacomenca.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;call super::losefocus;AcceptText()
end event

event rowfocuschanged;ib_datos_ok = True

IF This.RowCount() < 1 OR CurrentRow = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = CurrentRow
END IF
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!
		Message.DoubleParm = 0
		
		Parent.TriggerEvent("ue_validaregistro")
		
		IF Message.DoubleParm = -1 THEN
			This.SetRedraw(True)
			RETURN -1
		ELSEIF Key = KeyDownArrow! AND il_fila = dw_1.RowCount() THEN
			Parent.TriggerEvent("ue_nuevo_detalle")
		END IF
		
	CASE KeyTab!
		IF is_ultimacol = This.GetColumnName() AND il_fila = dw_1.RowCount() THEN
			Message.DoubleParm = 0
			
			Parent.TriggerEvent("ue_validaregistro")
			
			IF Message.DoubleParm = -1 THEN
				This.SetRedraw(True)
				RETURN -1
			ELSE
				Parent.TriggerEvent("ue_nuevo_detalle")
				
				This.SetFocus()
				This.SetRedraw(True)
				RETURN -1
			END IF
		END IF

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);il_fila	= This.GetRow()

RETURN 1
end event

event buttonclicked;call super::buttonclicked;Long 	ll_Fila
Dec{2}	ld_Bultos
Dec{3}	ld_KNetos
decimal	ldec_kilosant

CHOOSE CASE dwo.name
		CASE "b_envase"
		BuscaEnvase()
		
	CASE "b_calicosexero"
		BuscaCalidad()
		
	CASE "neto"
		//Boton provisorio para calcular peso neto del Lote
		IF NOT DestaraFila(row) THEN
			MessageBox("Error", "Ingrese todos los parametros antes de Destarar Bultos")
			This.Object.lfcd_kilnet[row]	=	0
		END IF
		
	CASE "romana"
		IF gstr_paramplanta.binsabins THEN
			
			IF (istr_puertacomm.pesajebins = 1 AND Dec(em_kilos.Text) >= istr_puertacomm.PesoMinimo) OR &
				(Dec(em_kilos.Text) >= 0) THEN	
				IF This.Object.bins_numero[il_Fila] > 0 THEN
					This.Object.lfcd_kilbru[row]	=	Dec(em_kilos.Text)
					ldec_kilosant 	= ObtieneKilosant(This.Object.fgmb_nrotar[row])
					ld_Bultos			=	This.Object.lfcd_bultos[row]
					This.Object.lfcd_kilnet[il_Fila]	=	Round((This.Object.lfcd_kilbru[row] - ldec_kilosant), 3)
					This.Object.lfcd_kilpro[il_Fila]	=	Round((This.Object.lfcd_kilbru[row] - ldec_kilosant) , 3)
				ELSE
					MessageBox("Atención", "Debe Ingresar Número de Bins Previamente")
					Return
				END IF
			END IF
		ELSE
			wstr_pesaje.puerta		=	istr_puertacomm
			wstr_pesaje.dw			=	dw_9
			
			wstr_pesaje.argum[1]	=	istr_mant.argumento[1]
			wstr_pesaje.argum[2]	=	istr_mant.argumento[2]
			wstr_pesaje.argum[3]	=	istr_mant.argumento[3]
			wstr_pesaje.Argum[7]	=	Istr_Mant.Argumento[6]			
			wstr_pesaje.argum[10]	=	String(dw_2.Object.clie_codigo[1])
			
			OpenWithParm(w_pesaje_romana,wstr_pesaje)
			
			wstr_pesaje	=	Message.PowerObjectParm
			
			This.Object.lfcd_kilnet[row]	=	0
			
			IF dw_9.RowCount() > 0 THEN
				FOR ll_Fila = 1 TO dw_9.RowCount()
					IF dw_9.Object.mfgp_estado[ll_Fila] = 2 THEN
						This.Object.lfcd_kilbru[row]	=	This.Object.lfcd_kilnet[row] + dw_9.Object.mfgp_pesore[ll_fila]
					END IF
				NEXT
			END IF
			
			ld_Bultos			=	This.Object.lfcd_bultos[row]
			
			This.Object.lfcd_kilnet[il_Fila]	=	Round((This.Object.lfcd_kilbru[row] - ldec_kilosant), 3)
			This.Object.lfcd_kilpro[il_Fila]	=	Round((This.Object.lfcd_kilbru[row] - ldec_kilosant) / ld_Bultos, 3)
	END IF
END CHOOSE
end event

event itemchanged;String	ls_Columna, ls_Null
Dec{2}	ld_Bultos
Dec{3}	ld_KNetos
Boolean  lb_ProdRetira
Integer	li_estado
Decimal	ldec_kilosant

ls_Columna = dwo.Name

SetNull(ls_Null)

CHOOSE CASE ls_Columna
	CASE "fgmb_nrotar"
		IF ExisteTarja(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], Long(data)) THEN
			This.SetItem(row, ls_columna, Long(ls_Null))
			This.SetColumn(ls_columna)
			This.SetFocus()
			RETURN 1
		ELSE
			il_tarjas[row]	=	Long(data)
		END IF
		
	CASE "bins_numero"
		IF NOT iuo_bins.Existe(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1], Long(data), sqlca, TRUE) THEN
			This.SetItem(row, ls_columna, Long(ls_Null))
			This.SetColumn(ls_columna)
			This.SetFocus()
			RETURN 1
		ELSE
			li_estado	=	iuo_bins.Estado(dw_2.Object.clie_codigo[1], dw_2.Object.plde_codigo[1],&
												 dw_4.Object.fgmb_nrotar[1], Long(data), sqlca)
			
			IF li_estado = 1 OR li_estado < 0 THEN
				MessageBox("Error", "El estado del bins no permite asociarlo a otro movimiento")
				This.SetItem(row, ls_columna, Long(ls_Null))
				This.SetColumn(ls_columna)
				This.SetFocus()
				RETURN 1
			ELSE
				THIS.Object.Enva_tipoen[row]	=	iuo_bins.enva_tipoen
				THIS.Object.Enva_codigo[row]	=	iuo_bins.Enva_codigo
				THIS.Object.enva_nombre[row]	=	iuo_bins.enva_nombre
			END IF
			
			il_bins[row]	=	Long(data)
		END IF
		
	CASE "enva_tipoen"
		IF NOT ExisteEnvase(Integer(Data), 0, istr_Envase) OR &
			istr_Envase.UsoEnvase <> 1 THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Null))
			RETURN 1
		ELSE
			istr_Mant.Argumento[8]	=	Data
		END IF

	CASE "enva_codigo"
		IF NOT ExisteEnvase(istr_Envase.TipoEnvase, Integer(Data), istr_Envase) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Null))
			This.SetItem(il_Fila, "enva_nombre", ls_Null)
			RETURN 1
		ELSE
			istr_Mant.Argumento[9]	=	Data
			This.Object.enva_nombre[il_Fila]	=	istr_Envase.Nombre
			IF idwc_calidad.Retrieve(This.Object.enva_tipoen[row], Integer(data)) = 0 THEN
				MessageBox("Error", "Envase " + String(This.Object.enva_tipoen[row], '0') + String(Integer(data), '000') + " no posee Tara. Seleccione Otro Envase")
			END IF
		END IF

	CASE "cate_codigo"
		IF NOT iuo_categorias.Existe(Integer(Data),True,SqlCa) THEN
			This.SetItem(il_fila, ls_Columna, Integer(ls_Null))
			RETURN 1
		ELSE
			// En comentario para poder ingresar datos Revisar si esta bien lo que
			// devuelve la funcion retiraprod
			
			//llama la funcion retiraprod, entregando los argumentos
			//y evaluando si retira el Prod. para asignar valor a "lofc_Tipool"
//			lb_ProdRetira = retiraprod(dw_3.Object.prod_codigo[1], &
//												dw_2.Object.espe_codigo[1], &
//												dw_2.Object.vari_codigo[1], &
//												integer(data), &
//												dw_2.Object.mfco_fecmov[1])
//												
//			IF Istr_Mant.Argumento[11] = '1' AND lb_ProdRetira = TRUE THEN
//				MessageBox("Error de Tipo de Pool","Ingreso No Corresponde al Inicial (Venta).~r Esta Información debe ser Ingresada en Otro Ventana.")
//				This.SetItem(il_fila, ls_Columna, Integer(ls_Null))
//				RETURN 1
//			ELSEIF Istr_Mant.Argumento[11] = '2' AND lb_ProdRetira = FALSE THEN
//					 MessageBox("Error de Tipo de Pool","Ingreso No Corresponde al Inicial (Retiro).~r Esta Información debe ser Ingresada en Otro Ventana.")
//				    This.SetItem(il_fila, ls_Columna, Integer(ls_Null))
//					 RETURN 1
//			END IF
		END IF
		
	CASE "lfcd_kilbru"
		IF gstr_paramplanta.binsabins THEN
			ldec_kilosant 	= ObtieneKilosant(This.Object.fgmb_nrotar[row])
		ELSE
			ldec_kilosant 	= 0
		END IF
		ld_Bultos			=	This.Object.lfcd_bultos[row]
		This.Object.lfcd_kilnet[il_Fila]	=	Round((Dec(Data) - ldec_kilosant), 3)
		This.Object.lfcd_kilpro[il_Fila]	=	Round((Dec(Data) - ldec_kilosant) / ld_Bultos, 3)

	CASE "cale_calida"
		IF iuo_calidad.Existe(THIS.object.enva_tipoen[row], THIS.object.enva_codigo[row], data, TRUE, sqlca) THEN
			THIS.Object.cale_nombre[row] = iuo_calidad.nombre
		ELSE
			This.SetItem(row, "cale_calida", ls_Null)
			Return 1
		END IF
		
END CHOOSE
end event

event itemerror;call super::itemerror;RETURN 1
end event

type tp_2 from userobject within tab_1
event create ( )
event destroy ( )
integer x = 18
integer y = 112
integer width = 3099
integer height = 864
boolean enabled = false
long backcolor = 12632256
string text = "Detalle de Envases       "
long tabtextcolor = 33554432
long tabbackcolor = 12632256
string picturename = "ArrangeTables!"
long picturemaskcolor = 553648127
dw_envases dw_envases
end type

on tp_2.create
this.dw_envases=create dw_envases
this.Control[]={this.dw_envases}
end on

on tp_2.destroy
destroy(this.dw_envases)
end on

type dw_envases from uo_dw within tp_2
integer x = 37
integer y = 36
integer width = 2981
integer height = 776
integer taborder = 31
string dataobject = "dw_mues_movtoenvadeta"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event clicked;call super::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event doubleclicked;call super::doubleclicked;w_maed_movtofrutacomer_proceso.TriggerEvent("ue_modifica_detalle")

RETURN 0
end event

event getfocus;call super::getfocus;IF il_fila > 0 THEN This.SelectRow(il_fila, True)

RETURN 0
end event

event itemfocuschanged;call super::itemfocuschanged;IF IsValid(w_main) THEN
	w_main.SetMicroHelp(dwo.Tag)
END IF
end event

event losefocus;call super::losefocus;AcceptText()

This.SelectRow(0, False)

RETURN 0
end event

event rowfocuschanged;call super::rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = getrow()
	This.SelectRow(0,False)
	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event type long dwnkey(keycode key, unsignedlong keyflags);call super::dwnkey;il_fila = This.GetRow()

This.SetRedraw(False)

CHOOSE CASE key
	CASE KeyEnter!
		w_maed_movtofrutacomer_proceso.PostEvent("ue_modifica_detalle")

	CASE KeyRightArrow!, KeyLeftArrow!
		This.SetRedraw(True)
		RETURN -1
		
	CASE KeyDownArrow!, KeyUpArrow!, KeyPageUp!, KeyPageDown!, KeyEnd!, KeyHome!
		w_maed_movtofrutacomer_proceso.PostEvent("ue_seteafila")

END CHOOSE

This.SetRedraw(True)

RETURN 0
end event

event type long ue_seteafila(unsignedlong wparam, long lparam);call super::ue_seteafila;il_fila	= This.GetRow()

This.SelectRow(0, False)
This.SelectRow(il_fila, True)

RETURN 0
end event

type em_kilos from editmask within w_maed_movtofrutacomer_granel
integer x = 2505
integer y = 780
integer width = 645
integer height = 120
integer taborder = 130
boolean bringtotop = true
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
borderstyle borderstyle = stylelowered!
string mask = "#,##0.00"
end type

type ole_puerta from olecustomcontrol within w_maed_movtofrutacomer_granel
event oncomm ( )
boolean visible = false
integer width = 174
integer height = 152
integer taborder = 120
boolean bringtotop = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "w_maed_movtofrutacomer_granel.win"
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
08w_maed_movtofrutacomer_granel.bin 
2B00000600e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe00000006000000000000000000000001000000010000000000001000fffffffe00000000fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
18w_maed_movtofrutacomer_granel.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
