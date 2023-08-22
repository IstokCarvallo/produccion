$PBExportHeader$w_mant_deta_spro_vaciadoproceso_bins_com.srw
$PBExportComments$Mantención Detalle de Bins vaciados a Proceso
forward
global type w_mant_deta_spro_vaciadoproceso_bins_com from w_mant_detalle_csd
end type
type st_1 from statictext within w_mant_deta_spro_vaciadoproceso_bins_com
end type
type sle_bruto from singlelineedit within w_mant_deta_spro_vaciadoproceso_bins_com
end type
type st_3 from statictext within w_mant_deta_spro_vaciadoproceso_bins_com
end type
type sle_neto from singlelineedit within w_mant_deta_spro_vaciadoproceso_bins_com
end type
type st_4 from statictext within w_mant_deta_spro_vaciadoproceso_bins_com
end type
type sle_kprom from singlelineedit within w_mant_deta_spro_vaciadoproceso_bins_com
end type
type cb_1 from commandbutton within w_mant_deta_spro_vaciadoproceso_bins_com
end type
type em_kilos from editmask within w_mant_deta_spro_vaciadoproceso_bins_com
end type
type st_fondo from statictext within w_mant_deta_spro_vaciadoproceso_bins_com
end type
type st_2 from statictext within w_mant_deta_spro_vaciadoproceso_bins_com
end type
type ole_puerta from olecustomcontrol within w_mant_deta_spro_vaciadoproceso_bins_com
end type
type str_pesaje from structure within w_mant_deta_spro_vaciadoproceso_bins_com
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

global type w_mant_deta_spro_vaciadoproceso_bins_com from w_mant_detalle_csd
integer width = 2245
integer height = 1464
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
global w_mant_deta_spro_vaciadoproceso_bins_com w_mant_deta_spro_vaciadoproceso_bins_com

type variables

Decimal 						id_pesosbins[], id_taraenvase
Integer						ii_estadoRetrieve
DataWindowChild			idwc_envases, idwc_calicosechero
Long							il_tarja
str_mant						istr_mant2

Private:
str_pesaje              		wstr_pesaje
str_puertacomm		      istr_puertacomm
end variables

forward prototypes
public subroutine calculapesos ()
public function boolean validatarja (long al_nrotarja)
public subroutine asignabultos ()
public subroutine pesooriginalbins ()
end prototypes

public subroutine calculapesos ();Decimal 	pesobruto, 	pesoneto, 		pesopromedio, ld_tarabasepallet
Integer 	li_fila,			li_TipoEnva, 	li_envase, 		li_Numero
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
			
	ls_Calidad = dw_1.Object.cale_calida[il_Fila]
	li_fila = idwc_CaliCosechero.Find("cale_calida ='" + ls_Calidad + "'", 1, idwc_CaliCosechero.RowCount()) 
	id_taraenvase 		= 	idwc_CaliCosechero.GetItemDecimal(li_fila, "cale_pesoen")
//	ld_tarabasepallet	=	id_taraenvase
	
	PesoBruto 		= 	dw_1.Object.opvd_pesobr[il_fila]
	PesoNeto 		=	pesobruto - (id_taraenvase * dw_1.Object.opvd_canbul[il_fila])
	PesoPromedio	=	PesoNeto / dw_1.Object.opvd_canbul[il_fila]
	
	dw_1.SetItem(il_fila, "opvd_pesone", PesoNeto)
	dw_1.SetItem(il_fila, "opvd_kilpro", PesoPromedio)
	
END IF
end subroutine

public function boolean validatarja (long al_nrotarja);Integer 	li_Cliente, li_Planta, li_lote, li_Especie, li_Envase, li_TipoEnva, li_TipOrden, li_Numero, li_fila
Long		ll_Bins, ll_cantidad
String	ls_Calidad

li_Cliente 	= 	Integer(istr_Mant.Argumento[1])
li_Planta 		= 	Integer(istr_Mant.Argumento[2])
li_TipOrden	=	Integer(istr_mant.Argumento[3])
li_Numero	=	Integer(istr_mant.Argumento[4])


li_fila = dw_1.Find("opve_nrtar1 = " + String(al_nrotarja) + " or opve_nrtar2 = " + String(al_nrotarja) &
					 + " or opve_nrtar3 = " + String(al_nrotarja), 1, dw_1.Rowcount())
IF li_fila > 0 THEN
	messagebox("Atención","La tarja  " + String(al_nrotarja, '00000000') + " esta ingresada en este Vaciado, ingrese otra Tarja")
	Return True		
END IF

SELECT Count(fgmb_nrotar)
INTO	:ll_cantidad
	FROM	dba.spro_movtobins
	WHERE 	clie_codigo		=	:li_Cliente
	  	AND	plde_codigo		=	:li_Planta
		AND	fgmb_nrotar		=	:al_nrotarja
		AND 	fgmb_estado	> 	0;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_movtobins" )
	Return True			
ELSEIF sqlca.SQLCode = 100 OR IsNull(ll_cantidad) OR ll_cantidad = 0 THEN
	messagebox("Atención","No Existe Tarja  " + String(al_nrotarja, '00000000') + " en Tabla de Movimiento de Bins o no esta Vigente")			
	Return True		
ELSEIF ll_cantidad > 1 THEN
	messagebox("Atención","La Tarja  " + String(al_nrotarja, '00000000') + " se encuentra repetida en Tabla de Movimiento de Bins")			
	Return True		
END IF

SELECT	bins_numero, lote_codigo, lote_espcod
	INTO	:ll_Bins, :li_Lote, :li_Especie
	FROM	dba.spro_movtobins
	WHERE 	clie_codigo		=	:li_Cliente
	  	AND	plde_codigo		=	:li_Planta
		AND	fgmb_nrotar		=	:al_nrotarja
		AND 	fgmb_estado	> 	0;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_movtobins" )
	Return True			
ELSEIF sqlca.SQLCode = 100 THEN
	messagebox("Atención","No Existe Tarja  " + String(al_nrotarja, '00000000') + " en Tabla de Movimiento de Bins o no esta Vigente")			
	Return True		
ELSE
	SELECT	enva_tipoen, enva_codigo
		INTO	:li_TipoEnva, :li_Envase 
		FROM	dba.spro_ordenprocdeta
		WHERE 	clie_codigo		=	:li_Cliente
			AND	plde_codigo		=	:li_Planta
			AND	orpr_numero	=	:li_Numero
			AND	orpr_tipord 		= 	:li_TipOrden
			AND 	lote_espcod 	= 	:li_Especie
			AND   lote_codigo 		= 	:li_Lote;
		
		IF sqlca.SQLCode = -1 THEN
			F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_ordenprocdeta" )
			Return True			
		ELSEIF sqlca.SQLCode = 100 THEN
			messagebox("Atención","No Existe la El Lote " + String(li_planta, '0000') + String(li_especie, '00') + String(li_Lote, '00000000') &
							+ " Para Orden de Proceso " + String(li_Numero, '00000'))			
			Return True		
		ELSE
			
			dw_1.GetChild("enva_codigo", idwc_envases)
			dw_1.GetChild("cale_calida", idwc_CaliCosechero)
			
			idwc_envases.SetTransObject(sqlca)
			idwc_CaliCosechero.SetTransObject(sqlca)
			
			idwc_envases.Retrieve(li_TipoEnva)
			idwc_CaliCosechero.Retrieve(li_TipoEnva, li_envase)
			
			SELECT	cale_calida
			INTO	:ls_Calidad 
			FROM	dba.spro_bins
			WHERE 	clie_codigo	=	:li_Cliente
				AND	plde_codigo	=	:li_Planta
				AND	bins_numero=	:ll_Bins;
				
			IF sqlca.SQLCode = -1 THEN
				F_ErrorBaseDatos(sqlca,"Lectura de la Tabla spro_ordenprocdeta" )
				Return True	
			ELSEIF sqlca.SQLCode = 100 THEN
				messagebox("Atención","No Existe la tara para esta base de Pallet " + String(al_nrotarja, '00000000'))
				Return True		
			END IF
			
			dw_1.Object.orpr_tipord[il_Fila]	=	Integer(istr_mant.Argumento[3])
			dw_1.Object.orpr_numero[il_Fila]	=	Integer(istr_mant.Argumento[4])			
			dw_1.Object.opvd_horava[il_Fila]	=	Time(f_fechahora())
			dw_1.Object.lote_pltcod[il_Fila] 	=	li_Planta
			dw_1.Object.lote_espcod[il_Fila] 	=	li_Especie
			dw_1.Object.lote_codigo[il_Fila]	=	li_Lote
			dw_1.Object.opve_fecvac[il_Fila]	=	Date(istr_mant.Argumento[7])
			dw_1.Object.enva_tipoen[il_Fila]	= 	li_TipoEnva
			dw_1.Object.enva_codigo[il_Fila]	= 	li_Envase
			dw_1.Object.cale_calida[il_Fila]	= 	ls_Calidad
			
			li_fila = idwc_CaliCosechero.Find("cale_calida ='" + ls_Calidad + "'", 1, idwc_CaliCosechero.RowCount()) 
			id_taraenvase = idwc_CaliCosechero.GetItemDecimal(li_fila, "cale_pesoen")
			
			RETURN False
		END IF
END IF
end function

public subroutine asignabultos ();Integer 	li_Bultos, li_cliente, li_planta, li_especie
Long		ll_lote, ll_tarja, ll_movimiento

li_cliente	=	Integer(istr_Mant.Argumento[1])
li_planta	=	Integer(istr_Mant.Argumento[2])


dw_1.AcceptText()
IF dw_1.Object.opve_nrtar1[il_fila] < 1 THEN RETURN

IF gstr_paramplanta.palletdebins THEN
	
	ll_tarja		=	dw_1.Object.opve_nrtar1[il_fila]
	li_especie	=	dw_1.Object.lote_espcod[il_Fila]
	ll_lote			=	dw_1.Object.lote_codigo[il_fila]
	
	SELECT mfge_numero INTO :ll_movimiento
		FROM dba.spro_movtobins
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
		FROM dba.spro_movtofrutagranpesa
		WHERE clie_codigo		=	:li_cliente
			AND plde_codigo		=	:li_planta
			AND tpmv_codigo		=	1
			AND mfge_numero	=	:ll_movimiento
			AND fgmb_nrotar		=	:ll_tarja;
			
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
	
ll_fila 			=	dw_1.GetRow() 

IF ll_fila < 1 THEN RETURN

ll_NroTar1	=	dw_1.Object.opve_nrtar1[ll_fila]
ll_NroTar2	=	dw_1.Object.opve_nrtar2[ll_fila]
ll_NroTar3	=	dw_1.Object.opve_nrtar3[ll_fila]
li_bultos		=	dw_1.Object.opvd_canbul[ll_fila]
li_TipEn		=	dw_1.Object.enva_tipoen[ll_fila]
li_CodEn		=	dw_1.Object.enva_codigo[ll_fila]
ls_Calid		=	dw_1.Object.cale_calida[ll_fila]
ll_lote			=	dw_1.Object.lote_codigo[ll_fila]

IF istr_mant2.Argumento[3] <> '9' THEN

	SELECT	Sum(mfgp.mfgp_pesore)
		INTO	:ldec_KilOri
		FROM	dba.spro_movtofrutagranpesa as mfgp
		WHERE 	mfgp.fgmb_nrotar IN (:ll_NroTar1, :ll_NroTar2, :ll_NroTar3)
			 AND	lote_codigo = :ll_lote;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_MovtoFrutaGranPesa")
	ELSE
		
		IF gstr_paramplanta.palletdebins THEN
			li_Cliente 	= 	Integer(istr_Mant.Argumento[1])
			li_Planta 		= 	Integer(istr_Mant.Argumento[2])
			li_Numero	=	Integer(istr_mant.Argumento[4])
			ll_lote			=	dw_1.Object.lote_codigo[il_fila]
			li_especie	=	dw_1.Object.lote_espcod[il_fila]
			il_tarja		=	dw_1.Object.opve_nrtar1[il_fila]
			
			SELECT enva_tipoen, enva_codigo, cale_calida
				INTO :li_TipoEnva, :li_Envase, :ls_Calidad
				FROM dba.spro_bins
				WHERE ( clie_codigo		=	:li_cliente)
					AND ( plde_codigo		=	:li_Planta	)
					AND ( bins_numero	=	:il_tarja	);
					
			IF sqlca.SQLCode < 0 THEN
				F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Movimiento de Envases")
			ELSEIF sqlca.SQLCode = 100 THEN
				MessageBox("Error", "No se encuentra el Movimiento de Envase Correspondiente a este Bulto")
				RETURN
			END IF
			
			SELECT cale_pesoen
				INTO 	:ld_tarabasepallet
				FROM 	dba.spro_calicosechero
				WHERE enva_tipoen	=	:li_TipoEnva
				  AND enva_codigo =  :li_Envase
				  AND cale_calida =  :ls_Calidad;
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Calidad de Envases")
				RETURN
			END IF
		END IF
		
		SELECT	cale_pesoen
		INTO	:ldec_Tara
		FROM	dba.spro_calicosechero
		WHERE  enva_tipoen	=	:li_TipEn
			AND  enva_codigo =  :li_CodEn
			AND  cale_calida =  :ls_Calid;
		 
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Calidad de Envases")
		ELSE
			dw_1.object.opvd_kilori[ll_fila]	=	Round(ldec_KilOri - (ldec_tara * li_bultos) - ld_tarabasepallet, 2)
		END IF	
	END IF
ELSE
	SELECT	Sum(lfgd.lfcd_kilnet)
		INTO	:ldec_KilOri
		FROM	dba.spro_lotesfrutacomdeta as lfgd
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

on w_mant_deta_spro_vaciadoproceso_bins_com.create
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

on w_mant_deta_spro_vaciadoproceso_bins_com.destroy
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

event open;call super::open;Integer	li_resultado
String		ls_parametros
Boolean	ib_OCX	=	True

istr_mant2		=	Message.PowerObjectParm

str_pesaje			lstr_pesaje
str_puertacomm 	lstr_puertacomm

wstr_pesaje		=	lstr_pesaje

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
		Ole_puerta.object.PortOpen	= True	
	END IF
END IF

IF gstr_paramplanta.palletdebins THEN
	dw_1.Object.opve_nrtar2.Protect		=	1
	dw_1.Object.opve_nrtar2.BackGround.Color	=	RGB(192,192,192)
	dw_1.Object.opve_nrtar3.Protect		=	1
	dw_1.Object.opve_nrtar3.BackGround.Color	=	RGB(192,192,192)
END IF


Timer(0.2)

end event

event ue_recuperadatos;call super::ue_recuperadatos;IF istr_mant.agrega = False and istr_mant.borra = False THEN
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
	dw_1.Object.kilos[il_fila]	=	ld_kilos
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.opvd_horava[il_Fila]	=	Time(f_fechahora())
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.Object.opve_nrtar1[il_Fila]	=	Long(ias_campo[1])
	dw_1.Object.opve_nrtar2[il_Fila]	=	Long(ias_campo[2])
	dw_1.Object.opve_nrtar3[il_Fila]	=	Long(ias_campo[3])
	dw_1.Object.opvd_pesone[il_Fila]	=	Long(ias_campo[4])
	dw_1.Object.opvd_pesobr[il_Fila]	=	Long(ias_campo[5])
	dw_1.Object.opvd_kilpro[il_Fila]	=	Long(ias_campo[6])
	dw_1.Object.enva_tipoen[il_Fila]	=	Long(ias_campo[7])
	dw_1.Object.enva_codigo[il_Fila]	=	Long(ias_campo[8])
	dw_1.Object.opvd_canbul[il_Fila]	=	Long(ias_campo[9])
	dw_1.Object.cale_calida[il_Fila]	=	ias_campo[10]
END IF
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_spro_vaciadoproceso_bins_com
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_spro_vaciadoproceso_bins_com
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_spro_vaciadoproceso_bins_com
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_spro_vaciadoproceso_bins_com
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_spro_vaciadoproceso_bins_com
integer x = 1989
integer y = 320
integer taborder = 70
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_spro_vaciadoproceso_bins_com
integer x = 1989
integer y = 140
integer taborder = 60
end type

event pb_acepta::clicked;IF dw_1.Object.opvd_canbul[il_fila] > 0 THEN
	dw_1.Object.opvd_horate[il_fila] = Time(f_fechahora())
	PesoOriginalBins()
	Call Super :: clicked
ELSE
	dw_1.SetFocus()
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_spro_vaciadoproceso_bins_com
integer x = 1989
integer y = 500
integer taborder = 80
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_spro_vaciadoproceso_bins_com
integer x = 32
integer y = 40
integer width = 1842
integer height = 1308
integer taborder = 30
string title = "Control Vaciado Detalle"
string dataobject = "dw_mant_deta_spro_ordenprocvacdeta"
end type

event dw_1::buttonclicked;call super::buttonclicked;string ls_columna

ls_columna = dwo.Name

CHOOSE CASE ls_columna

	CASE "botonromana"		
		IF (istr_puertacomm.pesajebins = 1 AND &
			This.Object.kilos[il_fila] >= istr_puertacomm.PesoMinimo) OR &
			(This.Object.kilos[il_fila] >= 0) THEN
				This.AcceptText()
				AsignaBultos()
				This.Object.opvd_pesobr[il_Fila]	=	This.Object.kilos[Row]
				CalculaPesos()
		END IF
		
END CHOOSE		
end event

event dw_1::itemchanged;call super::itemchanged;Long		ll_null
Integer	li_codigo, li_null, li_bultos, li_fila
String		ls_columna
Date     	ld_fecha

SetNull(ll_null)
SetNull(li_null)

ls_columna 	=	dwo.Name

CHOOSE CASE ls_columna
	CASE "opve_nrtar1","opve_nrtar2","opve_nrtar3"
		
			IF Not IsNull(data) THEN
				IF ValidaTarja(Long(Data)) THEN
					This.SetItem(row, ls_Columna, li_Null)
					Return 1
				ELSE
					This.Object.Lote[row] = String(This.Object.plde_codigo[row], '0000') &
												 + String(This.Object.lote_espcod[row], '00') &
												 + String(This.Object.lote_codigo[row], '0000000000')
					il_tarja	=	Long(Data)
				END IF	
			END IF
			
	CASE "orpr_tipord"	
		istr_mant.argumento[2] = Data
		dw_1.SetItem(1,"orpr_numero",ll_Null)
		
	CASE "opve_turno"
		istr_mant.argumento[5] = Data
		
	CASE "cale_calida"
		li_fila = idwc_CaliCosechero.Find("cale_calida ='" + data + "'", 1, idwc_CaliCosechero.RowCount()) 
		id_taraenvase = idwc_CaliCosechero.GetItemDecimal(li_fila, "cale_pesoen")
		
END CHOOSE

AsignaBultos()
CalculaPesos()
end event

type st_1 from statictext within w_mant_deta_spro_vaciadoproceso_bins_com
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

type sle_bruto from singlelineedit within w_mant_deta_spro_vaciadoproceso_bins_com
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

type st_3 from statictext within w_mant_deta_spro_vaciadoproceso_bins_com
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

type sle_neto from singlelineedit within w_mant_deta_spro_vaciadoproceso_bins_com
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

type st_4 from statictext within w_mant_deta_spro_vaciadoproceso_bins_com
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

type sle_kprom from singlelineedit within w_mant_deta_spro_vaciadoproceso_bins_com
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

type cb_1 from commandbutton within w_mant_deta_spro_vaciadoproceso_bins_com
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

type em_kilos from editmask within w_mant_deta_spro_vaciadoproceso_bins_com
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

type st_fondo from statictext within w_mant_deta_spro_vaciadoproceso_bins_com
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

type st_2 from statictext within w_mant_deta_spro_vaciadoproceso_bins_com
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

type ole_puerta from olecustomcontrol within w_mant_deta_spro_vaciadoproceso_bins_com
event oncomm ( )
boolean visible = false
integer x = 1984
integer y = 876
integer width = 174
integer height = 152
integer taborder = 100
boolean bringtotop = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "w_mant_deta_spro_vaciadoproceso_bins_com.win"
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
07w_mant_deta_spro_vaciadoproceso_bins_com.bin 
2500000c00e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe000000060000000000000000000000010000000100000000000010000000000200000001fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd00000004fffffffefffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffff000000030000000000000000000000000000000000000000000000000000000057bee7e001c7c4a700000003000000c00000000000500003004c004200430049004e0045004500530045004b000000590000000000000000000000000000000000000000000000000000000000000000000000000002001cffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000260000000000500003004f0042005800430054005300450052004d0041000000000000000000000000000000000000000000000000000000000000000000000000000000000002001affffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000010000003c00000000004200500043004f00530058004f00540041005200450047000000000000000000000000000000000000000000000000000000000000000000000000000000000101001a000000020000000100000004648a5600101b2c6e0000b682140000000000000057bee7e001c7c4a757bee7e001c7c4a7000000000000000000000000fffffffefffffffefffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
26ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00430079007000690072006800670020007400630028002000290039003100340039007200200065007600000072000e0006020c01d9774bfc28774bfc001234432100000008000003ed000003ed648a560100060000000100000000040000000200000025800008000000000000000000000000003f000000010073004d1234432100000008000003ed000003ed648a560100060000000100000000040000000200000025800008000000000000000000000000003f0000000100460064006c00690045006500690064006900740067006e0073005c0072006500650076000000720000000000250025020800f0033a6c70034399b80032006c0058002e004c004d0061005000730072007200650033002e0030002e0070005c006f0072006f0074006f0063005c006c0074005300460064006c00690045006500690064006900740067006e0073005c007200650065007600000072000e0006020c0106774bfc28774bfc000000000000000000000000000000000000000000033ddfa8000000020000000000140011020c010c0000000017c4becdffffffff00680004033de0180073004d006d00780032006c0058002e004c004d0061005000730072007200650033002e0030002e0070005c006f0072006f0074006f0063005c006c0074005300460064006c00690045006500690064006900740067006e0073005c006f00430074006e006e00650073007400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001020012ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000020000003c000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
17w_mant_deta_spro_vaciadoproceso_bins_com.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
