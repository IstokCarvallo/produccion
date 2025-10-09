$PBExportHeader$w_mant_deta_spro_vaciadoproceso_bins.srw
$PBExportComments$Mantención Detalle de Bins vaciados a Proceso
forward
global type w_mant_deta_spro_vaciadoproceso_bins from w_mant_detalle_csd
end type
type st_1 from statictext within w_mant_deta_spro_vaciadoproceso_bins
end type
type sle_bruto from singlelineedit within w_mant_deta_spro_vaciadoproceso_bins
end type
type st_3 from statictext within w_mant_deta_spro_vaciadoproceso_bins
end type
type sle_neto from singlelineedit within w_mant_deta_spro_vaciadoproceso_bins
end type
type st_4 from statictext within w_mant_deta_spro_vaciadoproceso_bins
end type
type sle_kprom from singlelineedit within w_mant_deta_spro_vaciadoproceso_bins
end type
type cb_1 from commandbutton within w_mant_deta_spro_vaciadoproceso_bins
end type
type em_kilos from editmask within w_mant_deta_spro_vaciadoproceso_bins
end type
type st_fondo from statictext within w_mant_deta_spro_vaciadoproceso_bins
end type
type st_2 from statictext within w_mant_deta_spro_vaciadoproceso_bins
end type
type ole_puerta from olecustomcontrol within w_mant_deta_spro_vaciadoproceso_bins
end type
type str_pesaje from structure within w_mant_deta_spro_vaciadoproceso_bins
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

global type w_mant_deta_spro_vaciadoproceso_bins from w_mant_detalle_csd
integer width = 2354
integer height = 1480
string title = "Ingreso de Detalle Vaciado de Bins"
st_1 st_1
sle_bruto sle_bruto
st_3 st_3
sle_neto sle_neto
st_4 st_4
sle_kprom sle_kprom
cb_1 cb_1
em_kilos em_kilos
st_fondo st_fondo
st_2 st_2
ole_puerta ole_puerta
end type
global w_mant_deta_spro_vaciadoproceso_bins w_mant_deta_spro_vaciadoproceso_bins

type variables

Decimal 						id_pesosbins[], id_taraenvase
Integer						ii_estadoRetrieve
DataWindowChild			idwc_envases, idwc_calicosechero
Long							il_tarja
str_mant						istr_mant2
uo_calicosechero			Iuo_calicosechero
uo_validacionesvaciado	iuo_valida

Private:
str_pesaje              wstr_pesaje
str_puertacomm	  istr_puertacomm
end variables

forward prototypes
public subroutine calculapesos ()
public function boolean validatarja (long al_nrotarja)
public subroutine asignabultos ()
public subroutine pesooriginalbins ()
public subroutine kilosbultobins ()
public subroutine kilospalletgranel ()
end prototypes

public subroutine calculapesos ();Decimal 	pesobruto, 		pesoneto, 		pesopromedio, 		ld_tarabasepallet
Integer 	li_fila,			li_TipoEnva, 	li_envase, 			li_Numero
Integer 	li_Cliente, 	li_Planta, 		li_lote, 			li_Especie
Long		ll_lote, 		ll_Bins,			ll_nrotarja
String 	ls_Calidad		

IF dw_1.Object.opvd_canbul[il_fila] > 0 THEN
	
	li_TipoEnva	=	dw_1.Object.enva_tipoen[il_Fila]
	li_Envase	=	dw_1.Object.enva_codigo[il_Fila]
			
	dw_1.GetChild("enva_codigo", idwc_envases)
	dw_1.GetChild("cale_calida", idwc_CaliCosechero)
	
	idwc_envases.SetTransObject(sqlca)
	idwc_CaliCosechero.SetTransObject(sqlca)
	
	idwc_envases.Retrieve(li_TipoEnva)
	idwc_CaliCosechero.Retrieve(li_TipoEnva, li_envase)
			
	ls_Calidad 	= 	dw_1.Object.cale_calida[il_Fila]
	li_fila 		= 	idwc_CaliCosechero.Find("cale_calida ='" + ls_Calidad + "'", 1, &
														idwc_CaliCosechero.RowCount()) 
	
	IF Not iuo_calicosechero.existe(li_TipoEnva, li_Envase, ls_Calidad, True, Sqlca) THEN
		RETURN
	ELSE
		id_taraenvase	=	iuo_calicosechero.Peso
	END IF
	
	IF gstr_paramplanta.palletdebins THEN
		
		li_Cliente 	= 	Integer(istr_Mant.Argumento[1])
		li_Planta 	= 	Integer(istr_Mant.Argumento[2])
		li_Numero	=	Integer(istr_mant.Argumento[4])
		ll_lote		=	dw_1.Object.lote_codigo[il_fila]
		li_especie	=	dw_1.Object.lote_espcod[il_fila]
		il_tarja		=	dw_1.Object.fgmb_tibapa[il_fila]
		
		IF il_tarja <> 0 AND NOT IsNull(il_tarja) THEN
			SELECT enva_tipoen, enva_codigo, cale_calida
			  INTO :li_TipoEnva, :li_Envase, :ls_Calidad
			  FROM dbo.spro_bins
			  WHERE clie_codigo	=	:li_cliente
				 AND plde_codigo	=	:li_Planta
				 AND bins_numero	=	:il_tarja;
		ELSE
			il_tarja	=	dw_1.Object.opve_nrtar1[il_fila]
			
			SELECT DISTINCT enva_tipoen, enva_codigo, cale_calida
			  INTO :li_TipoEnva, :li_Envase, :ls_Calidad
			  FROM dbo.spro_bins as bin, dbo.spro_movtofrutagranpesa as mgp
			  WHERE bin.clie_codigo	=	:li_cliente
				 AND bin.plde_codigo	=	:li_Planta
				 AND bin.bins_numero	=	mgp.mfgp_tibapa
				 and mgp.fgmb_nrotar =  :il_tarja
				 and mgp.clie_codigo	=	:li_cliente
				 AND mgp.plde_codigo	=	:li_Planta;
		
		END IF
		
		IF sqlca.SQLCode < 0 THEN
			F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento de Envases")
			
		ELSEIF sqlca.SQLCode = 100 THEN
			MessageBox("Error", "No se encuentra el Movimiento de Envase Correspondiente a este Bulto")
			RETURN
			
		END IF
		
		SELECT cale_pesoen
		  INTO 	:ld_tarabasepallet
		  FROM 	dbo.spro_calicosechero
		 WHERE enva_tipoen	=	:li_TipoEnva
			AND enva_codigo 	=  :li_Envase
			AND cale_calida 	=  :ls_Calidad;
			 
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Calidad de Envases")
			RETURN
		END IF
	
		id_taraenvase 	=	id_taraenvase + ( ld_tarabasepallet / dw_1.Object.opvd_canbul[il_fila] )
	END IF
	
	PesoBruto 		= 	dw_1.Object.opvd_pesobr[il_fila]
	PesoNeto 		=	pesobruto - (id_taraenvase * dw_1.Object.opvd_canbul[il_fila])
	PesoPromedio	=	PesoNeto / dw_1.Object.opvd_canbul[il_fila]
	
	dw_1.SetItem(il_fila, "opvd_pesone", PesoNeto)
	dw_1.SetItem(il_fila, "opvd_kilpro", PesoPromedio)
	
END IF
end subroutine

public function boolean validatarja (long al_nrotarja);Integer 	li_Cliente, li_Planta, li_lote, li_Especie, li_Envase, li_TipoEnva, li_TipOrden, li_Numero, li_fila, li_pltcod
Long		ll_Bins, ll_cantidad
String	ls_Calidad

li_Cliente 	= 	Integer(istr_Mant.Argumento[1])
li_Planta 		= 	Integer(istr_Mant.Argumento[2])
li_TipOrden	=	Integer(istr_mant.Argumento[3])
li_Numero	=	Integer(istr_mant.Argumento[4])

li_fila = dw_1.Find("opve_nrtar1 = " + String(al_nrotarja) + ' or ' + &
				    	  "opve_nrtar2 = " + String(al_nrotarja) + ' or ' + &
				    	  "opve_nrtar3 = " + String(al_nrotarja), 1, dw_1.Rowcount())
If li_fila > 0 Then
	MessageBox("Atención","La tarja  " + String(al_nrotarja, '00000000') + " esta ingresada en este Vaciado, ingrese otra Tarja.")
	Return True		
End If

If Not iuo_Valida.BinsDuplicados(li_Cliente, li_Planta, al_nrotarja, True, SQLCA) Then
	Return True		
End If

ll_bins		=	iuo_valida.Bins
li_lote			=	iuo_valida.Lote
li_especie	=	iuo_valida.Especie
li_pltcod		=	iuo_valida.plantalote
		
If Not iuo_valida.CargaBins(li_Cliente, li_Planta, ll_Bins, True, SQLCA) Then
	Return True		
End If

li_TipoEnva	=	iuo_valida.TipoEnva
li_Envase		=	iuo_valida.Envase
ls_Calidad 	=	iuo_valida.Calidad

 If iuo_valida.LoteTarja > 0 Then
	If Not iuo_valida.ValidaLote(li_Planta, li_Cliente, li_TipOrden, li_Numero, li_lote, li_TipoEnva, li_Envase,li_pltcod, True, SQLCA) Then
		Return True		
	End If	
 Else	
	If Not iuo_valida.ValidaLote(li_Planta, li_Cliente, li_TipOrden, li_Numero, li_lote, li_TipoEnva, li_Envase, True, SQLCA) Then
		Return True		
	End If
End If

If NOT iuo_valida.of_ValidaTarja(li_Planta, li_Cliente, al_nrotarja, True, SQLCA) Then Return True		

dw_1.GetChild("enva_codigo", idwc_envases)
dw_1.GetChild("cale_calida", idwc_CaliCosechero)

idwc_envases.SetTransObject(sqlca)
idwc_CaliCosechero.SetTransObject(sqlca)

idwc_envases.Retrieve(li_TipoEnva)
idwc_CaliCosechero.Retrieve(li_TipoEnva, li_envase)

If ( (dw_1.Object.enva_tipoen[il_Fila] <> li_TipoEnva) OR &
	  (dw_1.Object.enva_codigo[il_Fila] <> li_Envase)   OR &
	  (dw_1.Object.cale_calida[il_Fila] <> ls_Calidad)  OR &
	  (dw_1.Object.lote_codigo[il_Fila] <> li_Lote) ) AND &
	( dw_1.Object.opvd_canbul[il_fila] >= 1 ) Then
	MessageBox("Error", "Tarja Ingresada no cumple con la caracteristica de las Tarjas ya Ingresadas."+&
							  "~r~nIngrese o Seleccione otra.", Exclamation!)
	Return True
End If


dw_1.Object.orpr_tipord[il_Fila]	=	Integer(istr_mant.Argumento[3])
dw_1.Object.orpr_numero[il_Fila]	=	Integer(istr_mant.Argumento[4])			
dw_1.Object.opvd_horava[il_Fila]	=	Time(f_fechahora())
dw_1.Object.lote_pltcod[il_Fila] =	iuo_valida.plantalote
dw_1.Object.lote_espcod[il_Fila] =	li_Especie
dw_1.Object.lote_codigo[il_Fila]	=	li_Lote
dw_1.Object.opve_fecvac[il_Fila]	=	Date(istr_mant.Argumento[7])
dw_1.Object.enva_tipoen[il_Fila]	= 	li_TipoEnva
dw_1.Object.enva_codigo[il_Fila]	= 	li_Envase
dw_1.Object.cale_calida[il_Fila]	= 	ls_Calidad
dw_1.Object.fgmb_tibapa[il_Fila]	= 	iuo_valida.fgmb_tibapa

li_fila 			= idwc_CaliCosechero.Find("cale_calida ='" + ls_Calidad + "'", 1, idwc_CaliCosechero.RowCount()) 
id_taraenvase 	= idwc_CaliCosechero.GetItemDecimal(li_fila, "cale_pesoen")

Return False

end function

public subroutine asignabultos ();Integer 	li_Bultos, li_cliente, li_planta, li_especie
Long		ll_lote, ll_tarja, ll_movimiento
Date		ld_hoy

li_cliente	=	Integer(istr_Mant.Argumento[1])
li_planta	=	Integer(istr_Mant.Argumento[2])

select getdate() 
  into :ld_hoy 
  from dummy;

IF dw_1.GetItemStatus(il_fila, 0, Primary!) = NewModified! THEN
	IF DaysAfter(Date(istr_mant2.Argumento[7]), ld_hoy) > 1 THEN
		dw_1.Object.opvd_fereva[il_fila]	=	Date(istr_mant2.Argumento[7])
	ELSE
		dw_1.Object.opvd_fereva[il_fila]	=	ld_hoy
	END IF
END IF

dw_1.AcceptText()
IF dw_1.Object.opve_nrtar1[il_fila] < 1 THEN RETURN

IF gstr_paramplanta.palletdebins THEN
	
	ll_tarja		=	dw_1.Object.opve_nrtar1[il_fila]
	li_especie	=	dw_1.Object.lote_espcod[il_Fila]
	ll_lote		=	dw_1.Object.lote_codigo[il_fila]
	
	SELECT mfge_numero INTO :ll_movimiento
		FROM dbo.spro_movtobins
		WHERE clie_codigo	=	:li_cliente
		  AND plde_codigo	=	:li_planta
		  AND fgmb_nrotar	=	:ll_tarja
		  AND lote_espcod	=	:li_especie
		  AND lote_codigo	=	:ll_lote;
			
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_movtobins" )
		li_Bultos 	= 	0
	ELSEIF sqlca.SQLCode = 100 THEN
		messagebox("Atención","No Existe la Tarja " + String(ll_tarja, '00000000'))			
		li_Bultos 	= 	0
	ELSE
		SELECT Count(*) INTO :li_bultos
		FROM dbo.spro_movtofrutagranpesa
		WHERE clie_codigo	=	:li_cliente
		 AND plde_codigo	=	:li_planta
		 AND tpmv_codigo	in	(1, 2)
		 AND mfge_numero	=	:ll_movimiento
		 AND fgmb_nrotar	=	:ll_tarja;
			
		IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_movtofrutagranpesa" )
			li_Bultos 	= 	0
		ELSEIF sqlca.SQLCode = 100 THEN
			messagebox("Atención","No Se encuentran Bultos para esta especificacion, ~r Favor de dar aviso a Administrador de Sistemas")			
			li_Bultos 	= 	0
		END IF
	END IF
ELSE
	
	IF dw_1.Object.opve_nrtar1[il_fila] > 0 THEN li_Bultos ++
	IF dw_1.Object.opve_nrtar2[il_fila] > 0 THEN li_Bultos ++
	IF dw_1.Object.opve_nrtar3[il_fila] > 0 THEN li_Bultos ++
	
END IF

dw_1.Object.opvd_canbul[il_fila] = li_bultos
end subroutine

public subroutine pesooriginalbins ();Long 		ll_NroTar1, ll_NroTar2, ll_NroTar3, ll_fila, ll_lote
Integer	li_TipEn, li_CodEn, li_bultos, li_Cliente, li_Planta, li_Numero, li_especie, li_tipoenva, li_envase
String		ls_Calid, ls_calidad
Decimal	ldec_KilOri, ldec_Tara,ld_tarabasepallet
	
ll_fila 			=	il_Fila//dw_1.GetRow() 

IF ll_fila < 1 THEN RETURN

ll_NroTar1	=	dw_1.Object.opve_nrtar1[ll_fila]
ll_NroTar2	=	dw_1.Object.opve_nrtar2[ll_fila]
ll_NroTar3	=	dw_1.Object.opve_nrtar3[ll_fila]
li_bultos		=	dw_1.Object.opvd_canbul[ll_fila]
li_TipEn		=	dw_1.Object.enva_tipoen[ll_fila]
li_CodEn		=	dw_1.Object.enva_codigo[ll_fila]
ls_Calid		=	dw_1.Object.cale_calida[ll_fila]
li_especie	=	dw_1.Object.lote_espcod[il_fila]
ll_lote			=	dw_1.Object.lote_codigo[ll_fila]
li_Planta		=	dw_1.Object.lote_pltcod[ll_fila]

IF istr_mant2.Argumento[3] <> '9' THEN

	SELECT Sum(mfgp.mfgp_pesore)
	  INTO :ldec_KilOri
	  FROM dbo.spro_movtofrutagranpesa as mfgp
	 WHERE mfgp.fgmb_nrotar IN (:ll_NroTar1, :ll_NroTar2, :ll_NroTar3)
		AND lote_codigo = :ll_lote
		AND lote_espcod = :li_especie	
		AND lote_pltcod = :li_Planta;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_MovtoFrutaGranPesa")
	ELSE
		
		IF gstr_paramplanta.palletdebins THEN
			li_Cliente 	= 	Integer(istr_Mant.Argumento[1])
			li_Planta 	= 	Integer(istr_Mant.Argumento[2])
			li_Numero	=	Integer(istr_mant.Argumento[4])
			ll_lote		=	dw_1.Object.lote_codigo[il_fila]
			li_especie	=	dw_1.Object.lote_espcod[il_fila]
			il_tarja		=	dw_1.Object.fgmb_tibapa[il_fila]
			
			SELECT enva_tipoen, enva_codigo, cale_calida
			  INTO :li_TipoEnva, :li_Envase, :ls_Calidad
			  FROM dbo.spro_bins
			 WHERE( clie_codigo	=	:li_cliente	)
				AND( plde_codigo	=	:li_Planta	)
				AND( bins_numero	=	:il_tarja	);
					
			IF sqlca.SQLCode < 0 THEN
				F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento de Envases")
			ELSEIF sqlca.SQLCode = 100 THEN
				MessageBox("Error", "No se encuentra el Movimiento de Envase Correspondiente a este Bulto")
				RETURN
			END IF
			
			SELECT cale_pesoen
			  INTO :ld_tarabasepallet
			  FROM dbo.spro_calicosechero
			 WHERE enva_tipoen =	 :li_TipoEnva
				AND enva_codigo =  :li_Envase
				AND cale_calida =  :ls_Calidad;
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Calidad de Envases")
				RETURN
			END IF
		END IF
		
		SELECT cale_pesoen
		  INTO :ldec_Tara
		  FROM dbo.spro_calicosechero
		 WHERE enva_tipoen = :li_TipEn
			AND enva_codigo = :li_CodEn
			AND cale_calida = :ls_Calid;
		 
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Calidad de Envases")
		ELSE
			dw_1.object.opvd_kilori[ll_fila]	=	Round(ldec_KilOri - (ldec_tara * li_bultos) - ld_tarabasepallet, 2)
		END IF	
	END IF
ELSE
	SELECT	Sum(lfgd.lfcd_kilnet)
		INTO	:ldec_KilOri
		FROM	dbo.spro_lotesfrutacomdeta as lfgd
		WHERE 	lfgd.fgmb_nrotar IN (:ll_NroTar1, :ll_NroTar2, :ll_NroTar3)
			 AND	lfgd.lofc_lotefc = :ll_lote;
			 
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Lotes Comerciales")
		dw_1.object.opvd_kilori[ll_fila]	=	0
	ELSE
		dw_1.object.opvd_kilori[ll_fila]	=	Round(ldec_KilOri, 2)
	END IF	

END IF
end subroutine

public subroutine kilosbultobins ();Long 		ll_NroTar1, ll_NroTar2, ll_NroTar3, ll_fila, ll_lote
Integer	li_TipEn, li_CodEn, li_bultos, li_Cliente, li_Planta, li_Numero, li_especie, li_tipoenva, li_envase
String		ls_Calid, ls_calidad
Decimal	ldec_KilOri, ldec_Tara,ld_tarabasepallet
	
ll_fila 			=	il_fila//dw_1.GetRow() 

IF ll_fila < 1 THEN RETURN

ll_NroTar1	=	dw_1.Object.opve_nrtar1[ll_fila]
ll_NroTar2	=	dw_1.Object.opve_nrtar2[ll_fila]
ll_NroTar3	=	dw_1.Object.opve_nrtar3[ll_fila]
li_bultos		=	dw_1.Object.opvd_canbul[ll_fila]
li_TipEn		=	dw_1.Object.enva_tipoen[ll_fila]
li_CodEn		=	dw_1.Object.enva_codigo[ll_fila]
ls_Calid		=	dw_1.Object.cale_calida[ll_fila]
ll_lote			=	dw_1.Object.lote_codigo[ll_fila]

SELECT Sum(mfgp.mfgp_pesore)
  INTO :ldec_KilOri
  FROM dbo.spro_movtofrutagranpesa as mfgp
 WHERE mfgp.fgmb_nrotar IN (:ll_NroTar1, :ll_NroTar2, :ll_NroTar3)
	AND lote_codigo = :ll_lote;
	
IF sqlca.SQLCode <> 0 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_MovtoFrutaGranPesa")
ELSE

	SELECT cale_pesoen
	  INTO :ldec_Tara
	  FROM dbo.spro_calicosechero
	 WHERE enva_tipoen = :li_TipEn
		AND enva_codigo = :li_CodEn
		AND cale_calida = :ls_Calid;
	 
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Calidad de Envases")
	ELSE
		dw_1.object.opvd_kilori[ll_fila]	=	Round(ldec_KilOri - (ldec_tara * li_bultos) - ld_tarabasepallet, 2)
		dw_1.Object.opvd_pesone[ll_fila]	=	dw_1.object.opvd_kilori[ll_fila]
		dw_1.Object.opvd_pesobr[ll_fila]	=	ldec_KilOri
		dw_1.Object.opvd_kilpro[ll_fila]	=	Round( Round(ldec_KilOri - (ldec_tara * li_bultos) - ld_tarabasepallet, 2) / dw_1.Object.opvd_canbul[il_fila], 2)
		dw_1.Object.kilos[ll_fila]			=	ldec_KilOri
	END IF
END IF
end subroutine

public subroutine kilospalletgranel ();Long 		ll_NroTar1, ll_NroTar2, ll_NroTar3, ll_fila, ll_lote
Integer	li_TipEn, li_CodEn, li_bultos, li_Cliente, li_Planta, li_Numero, li_especie, li_tipoenva, li_envase, li_baspal
String		ls_Calid, ls_calidad
Decimal	ldec_KilOri, ldec_Tara,ld_tarabasepallet
	
ll_fila 			=	il_Fila//dw_1.GetRow() 

IF ll_fila < 1 THEN RETURN

ll_NroTar1	=	dw_1.Object.opve_nrtar1[ll_fila]
li_bultos	=	dw_1.Object.opvd_canbul[ll_fila]
li_TipEn		=	dw_1.Object.enva_tipoen[ll_fila]
li_CodEn		=	dw_1.Object.enva_codigo[ll_fila]
ls_Calid		=	dw_1.Object.cale_calida[ll_fila]
ll_lote		=	dw_1.Object.lote_codigo[ll_fila]

SELECT Sum(mfgp.mfgp_pesore), mfgp_tibapa
  INTO :ldec_KilOri, :li_baspal
  FROM dbo.spro_movtofrutagranpesa as mfgp
 WHERE mfgp.fgmb_nrotar = :ll_NroTar1
	AND lote_codigo = :ll_lote
 group by mfgp_tibapa;
	
IF sqlca.SQLCode <> 0 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_MovtoFrutaGranPesa")
ELSE

	SELECT cale_pesoen
	  INTO :ldec_Tara
	  FROM dbo.spro_calicosechero
	 WHERE enva_tipoen = :li_TipEn
		AND enva_codigo = :li_CodEn
		AND cale_calida = :ls_Calid;
	 
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Calidad de Envases")
	ELSE
		dw_1.object.opvd_kilori[ll_fila]		=	Round(ldec_KilOri - (ldec_tara * li_bultos) - ld_tarabasepallet, 2)
		dw_1.Object.opvd_pesone[ll_fila]	=	dw_1.object.opvd_kilori[ll_fila]
		dw_1.Object.opvd_pesobr[ll_fila]	=	ldec_KilOri
		dw_1.Object.opvd_kilpro[ll_fila]	=	Round( Round(ldec_KilOri - (ldec_tara * li_bultos) - ld_tarabasepallet, 2) / dw_1.Object.opvd_canbul[il_fila], 2)
		dw_1.Object.kilos[ll_fila]				=	ldec_KilOri
	END IF
END IF
end subroutine

on w_mant_deta_spro_vaciadoproceso_bins.create
int iCurrent
call super::create
this.st_1=create st_1
this.sle_bruto=create sle_bruto
this.st_3=create st_3
this.sle_neto=create sle_neto
this.st_4=create st_4
this.sle_kprom=create sle_kprom
this.cb_1=create cb_1
this.em_kilos=create em_kilos
this.st_fondo=create st_fondo
this.st_2=create st_2
this.ole_puerta=create ole_puerta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.sle_bruto
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.sle_neto
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.sle_kprom
this.Control[iCurrent+7]=this.cb_1
this.Control[iCurrent+8]=this.em_kilos
this.Control[iCurrent+9]=this.st_fondo
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.ole_puerta
end on

on w_mant_deta_spro_vaciadoproceso_bins.destroy
call super::destroy
destroy(this.st_1)
destroy(this.sle_bruto)
destroy(this.st_3)
destroy(this.sle_neto)
destroy(this.st_4)
destroy(this.sle_kprom)
destroy(this.cb_1)
destroy(this.em_kilos)
destroy(this.st_fondo)
destroy(this.st_2)
destroy(this.ole_puerta)
end on

event open;Integer	li_resultado
String	ls_parametros
Boolean	ib_OCX	=	True

//LONG ll_RC 
//Funcion que llamam a la opción de registro dinamico de dlls
//ll_RC 				= 	DllRegisterServer()

dw_1.GetChild("enva_codigo", idwc_envases)
idwc_envases.SetTransObject(sqlca)
idwc_envases.InsertRow(0)

dw_1.GetChild("cale_calida", idwc_CaliCosechero)
idwc_CaliCosechero.SetTransObject(sqlca)
idwc_CaliCosechero.InsertRow(0)

Call Super::Open

istr_mant2			=	Message.PowerObjectParm

str_pesaje			lstr_pesaje
str_puertacomm 	lstr_puertacomm

wstr_pesaje			=	lstr_pesaje

li_resultado 		=	ConfiguracionPuerta(istr_puertacomm)

IF li_resultado 	= 	0 THEN
	ls_parametros	=	String(istr_puertacomm.Baudios)+","+&
							istr_puertacomm.Paridad+","+&
							String(istr_puertacomm.Data)+","+&
							String(istr_puertacomm.Parada)
			
	IF NOT ib_OCX THEN
		MessageBox("Conexión Romana","No está instalado el OCX para conexión con Romana")
	ELSE
//		IF Ole_puerta.object.PortOpen THEN Ole_puerta.object.PortOpen = False
//		Ole_puerta.object.settings	=	ls_parametros
//		Ole_puerta.object.PortOpen	= True	
	END IF
END IF

IF gstr_paramplanta.palletdebins THEN
	dw_1.Object.opve_nrtar2.Protect				=	1
	dw_1.Object.opve_nrtar2.Color					=	0
	dw_1.Object.opve_nrtar2.BackGround.Color	=	553648127
	dw_1.Object.opve_nrtar3.Protect				=	1
	dw_1.Object.opve_nrtar3.Color					=	0
	dw_1.Object.opve_nrtar3.BackGround.Color	=	553648127
END IF

Timer(0.2)

iuo_calicosechero	=	Create	uo_calicosechero
iuo_valida			=	Create 	uo_validacionesvaciado
end event

event ue_recuperadatos;call super::ue_recuperadatos;If Not istr_mant.Agrega And Not istr_mant.Borra Then
	ias_campo[1]  = String(dw_1.GetItemNumber(il_fila,"opve_nrtar1"))
	ias_campo[2]  = String(dw_1.GetItemNumber(il_fila,"opve_nrtar2"))
	ias_campo[3]  = String(dw_1.GetItemNumber(il_fila,"opve_nrtar3"))
	ias_campo[4]  = String(dw_1.GetItemNumber(il_fila,"opvd_pesone"))
	ias_campo[5]  = String(dw_1.GetItemNumber(il_fila,"opvd_pesobr"))
	ias_campo[6]  = String(dw_1.GetItemNumber(il_fila,"opvd_kilpro"))
	ias_campo[7]  = String(dw_1.GetItemNumber(il_fila,"enva_tipoen"))
	ias_campo[8]  = String(dw_1.GetItemNumber(il_fila,"enva_codigo"))
	ias_campo[9]  = String(dw_1.GetItemNumber(il_fila,"opvd_canbul"))
	ias_campo[10] = dw_1.GetItemString(il_fila,"cale_calida")
END IF


end event

event closequery;call super::closequery;IF dec(sle_neto.text) <= 0 THEN
//	MessageBox("Error", "Los pesos no Ingresados", StopSign!)
//	Return 1
ELSE
	RETURN 0
END IF
end event

event timer;call super::timer;Integer	li_factor, li_posini, li_LarBuf
String 	ls_string
Double	ld_kilos

//Ole_Puerta.Object.inputlen =	Integer(istr_puertacomm.LargoLectura)
//
//li_LarBuf =Ole_Puerta.Object.InBufferCount
//
//IF li_LarBuf > 0 THEN
//	ls_string =  Ole_Puerta.Object.input
//END IF
//
//li_posini = Pos(ls_string,istr_puertacomm.CadenaInicio) + Len(istr_puertacomm.CadenaInicio)
//
//IF Len(Mid(ls_string,li_posini)) < istr_puertacomm.LargoCadena THEN RETURN
//	
//IF IsNumber(Mid(ls_string,li_posini,istr_puertacomm.LargoCadena)) THEN
//	ld_kilos	=	Dec(Mid(ls_string,li_posini,istr_puertacomm.LargoCadena))
//	IF istr_puertacomm.Decimales > 0 THEN
//		li_factor	= 10 ^ istr_puertacomm.Decimales
//		ld_kilos		= Round(ld_kilos/li_factor,istr_puertacomm.Decimales)
//	END IF
//	dw_1.Object.kilos[il_fila]	=	ld_kilos
//END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.opvd_horava[il_Fila]	=	Time(f_fechahora())
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.Object.opve_nrtar1[il_Fila]	=	Long(ias_campo[1])
	dw_1.Object.opve_nrtar2[il_Fila]	=	Long(ias_campo[2])
	dw_1.Object.opve_nrtar3[il_Fila]	=	Long(ias_campo[3])
	dw_1.Object.opvd_pesone[il_Fila]	=	Dec(ias_campo[4])
	dw_1.Object.opvd_pesobr[il_Fila]	=	Dec(ias_campo[5])
	dw_1.Object.opvd_kilpro[il_Fila]	=	Dec(ias_campo[6])
	dw_1.Object.enva_tipoen[il_Fila]	=	Long(ias_campo[7])
	dw_1.Object.enva_codigo[il_Fila]	=	Long(ias_campo[8])
	dw_1.Object.opvd_canbul[il_Fila]	=	Long(ias_campo[9])
	dw_1.Object.cale_calida[il_Fila]	=	ias_campo[10]
END IF
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_spro_vaciadoproceso_bins
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_spro_vaciadoproceso_bins
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_spro_vaciadoproceso_bins
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_spro_vaciadoproceso_bins
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_spro_vaciadoproceso_bins
integer x = 1989
integer y = 320
integer taborder = 70
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_spro_vaciadoproceso_bins
integer x = 1989
integer y = 140
integer taborder = 60
end type

event pb_acepta::clicked;IF dw_1.Object.opvd_canbul[il_fila] > 0 THEN
	IF dw_1.GetItemStatus(il_fila, 0, Primary!) = NewModified! THEN
		dw_1.Object.opvd_horate[il_fila] = Time(f_fechahora())
	END IF
	
	PesoOriginalBins()
	Call Super :: clicked
ELSE
	dw_1.SetFocus()
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_spro_vaciadoproceso_bins
integer x = 1989
integer y = 500
integer taborder = 80
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_spro_vaciadoproceso_bins
integer x = 78
integer y = 104
integer width = 1774
integer height = 1248
integer taborder = 30
string title = "Control Vaciado Detalle"
string dataobject = "dw_mant_deta_spro_ordenprocvacdeta"
end type

event dw_1::buttonclicked;call super::buttonclicked;string ls_columna
Integer li_Resp

ls_columna = dwo.Name

Choose Case ls_columna
	Case "botonromana"		
		If (istr_puertacomm.pesajebins = 1 And This.Object.kilos[il_fila] >= istr_puertacomm.PesoMinimo) OR &
			(This.Object.kilos[il_fila] >= 0) Then
				This.AcceptText()
				AsignaBultos()
				This.Object.opvd_pesobr[il_Fila]	=	This.Object.kilos[Row]
				If gstr_paramplanta.bultobins Then
					 If MessageBox("Atención", "Desea Utilizar Peso de Recepción", Exclamation!, YesNo!,1) = 1 Then
						KilosBultoBins()
					Else
						CalculaPesos()
					End If
				Else
					CalculaPesos()
				End If
		ElseIf gstr_paramplanta.bultobins Then
			 If MessageBox("Atención", "Desea Utilizar Peso de Recepción", Exclamation!, YesNo!,1) = 1 Then
				KilosBultoBins()
			Else
				CalculaPesos()
			End If
		End If
End Choose		
end event

event dw_1::itemchanged;call super::itemchanged;Integer	li_codigo, li_bultos, li_fila
String		ls_columna, ls_Null
Date     	ld_fecha

SetNull(ls_Null)

ls_columna 	=	dwo.Name
Choose Case ls_columna
	Case "opve_nrtar1", "opve_nrtar2", "opve_nrtar3"
			If Not IsNull(data) Then
				If ValidaTarja(Long(Data)) Then
					This.SetItem(row, ls_Columna, Integer(ls_Null))
					Return 1
				Else
					This.Object.Lote[row] 	= 	String(This.Object.plde_codigo[row], '0000') &
												 	+ 	String(This.Object.lote_espcod[row], '00') &
													+ 	String(This.Object.lote_codigo[row], '0000000000')
					il_tarja						=	Long(Data)
				End If	
			End If
			
	Case "orpr_tipord"	
		istr_mant.argumento[2] = Data
		dw_1.SetItem(1,"orpr_numero",Long(ls_Null))
		
	Case "opve_turno"
		istr_mant.argumento[5] = Data
		
	Case "cale_calida"
		li_fila = idwc_CaliCosechero.Find("cale_calida ='" + data + "'", 1, idwc_CaliCosechero.RowCount()) 
		id_taraenvase = idwc_CaliCosechero.GetItemDecimal(li_fila, "cale_pesoen")
		
End Choose 

AsignaBultos()
CalculaPesos()
end event

type st_1 from statictext within w_mant_deta_spro_vaciadoproceso_bins
boolean visible = false
integer x = 809
integer y = 1412
integer width = 256
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
string text = "P. Bruto"
boolean focusrectangle = false
end type

type sle_bruto from singlelineedit within w_mant_deta_spro_vaciadoproceso_bins
boolean visible = false
integer x = 736
integer y = 1496
integer width = 402
integer height = 88
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

event modified;IF IsNumber(this.Text) THEN
	calculapesos()
ELSE
	This.Text = "0"
	This.SetFocus()
END IF
end event

type st_3 from statictext within w_mant_deta_spro_vaciadoproceso_bins
boolean visible = false
integer x = 1225
integer y = 1412
integer width = 256
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
string text = "P. Neto"
boolean focusrectangle = false
end type

type sle_neto from singlelineedit within w_mant_deta_spro_vaciadoproceso_bins
boolean visible = false
integer x = 1152
integer y = 1496
integer width = 402
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

type st_4 from statictext within w_mant_deta_spro_vaciadoproceso_bins
boolean visible = false
integer x = 1632
integer y = 1412
integer width = 256
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
string text = "K. Prom"
boolean focusrectangle = false
end type

type sle_kprom from singlelineedit within w_mant_deta_spro_vaciadoproceso_bins
boolean visible = false
integer x = 1559
integer y = 1496
integer width = 402
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

type cb_1 from commandbutton within w_mant_deta_spro_vaciadoproceso_bins
boolean visible = false
integer x = 603
integer y = 1484
integer width = 87
integer height = 72
integer taborder = 50
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "R"
end type

event clicked;
IF istr_puertacomm.pesajebins = 1 and false = TRUE THEN
	sle_bruto.Text	=	em_kilos.Text
	
ELSE
	IF Dec(em_kilos.Text) >= istr_puertacomm.PesoMinimo THEN
		sle_bruto.Text	=	String(Dec(em_kilos.Text))
	END IF
END IF
	
end event

type em_kilos from editmask within w_mant_deta_spro_vaciadoproceso_bins
boolean visible = false
integer x = 137
integer y = 1452
integer width = 430
integer height = 120
integer taborder = 120
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

type st_fondo from statictext within w_mant_deta_spro_vaciadoproceso_bins
boolean visible = false
integer x = 114
integer y = 1432
integer width = 480
integer height = 164
integer textsize = -20
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "System"
long textcolor = 33554432
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_deta_spro_vaciadoproceso_bins
boolean visible = false
integer x = 59
integer y = 1400
integer width = 1961
integer height = 268
integer textsize = -10
integer weight = 400
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

type ole_puerta from olecustomcontrol within w_mant_deta_spro_vaciadoproceso_bins
event oncomm ( )
boolean visible = false
integer x = 1989
integer y = 916
integer width = 174
integer height = 152
integer taborder = 90
boolean bringtotop = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "w_mant_deta_spro_vaciadoproceso_bins.win"
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
09w_mant_deta_spro_vaciadoproceso_bins.bin 
2B00000600e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe00000006000000000000000000000001000000010000000000001000fffffffe00000000fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
19w_mant_deta_spro_vaciadoproceso_bins.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
