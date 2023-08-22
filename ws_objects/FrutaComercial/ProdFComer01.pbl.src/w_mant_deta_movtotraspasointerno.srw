$PBExportHeader$w_mant_deta_movtotraspasointerno.srw
$PBExportComments$Mantención Detalle de Movimiento de Fruta Comercial.
forward
global type w_mant_deta_movtotraspasointerno from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_movtotraspasointerno from w_mant_detalle_csd
integer width = 2670
integer height = 2048
end type
global w_mant_deta_movtotraspasointerno w_mant_deta_movtotraspasointerno

type variables
uo_plantadesp			iuo_PltaLote
uo_especie				iuo_Especie
uo_lotesfrutacomer	iuo_LoteFC
uo_productores			iuo_Productor
uo_camarasfrigo		iuo_Camara
uo_tipomovtofruta		iuo_TipoMovto

str_variedad			istr_Variedad
str_envase				istr_Envase

DataWindowChild		idwc_Camara, idwc_Planta, idwc_Especie
DataWindowChild		idwc_tipoenvase  //agrego lm
end variables

forward prototypes
public function boolean existelote (string as_columna, string as_valor)
public function boolean duplicado (string as_columna, string as_valor)
public subroutine buscaenvase ()
public function boolean hayexistencia (string as_columna, string as_valor)
public subroutine buscalote ()
end prototypes

public function boolean existelote (string as_columna, string as_valor);Long		ll_Fila
Integer	li_Planta, li_Especie, li_Lote, li_Secuencia
Boolean	lb_Retorno	=	True

li_Planta		=	dw_1.Object.lofc_pltcod[il_Fila]
li_Especie		=	dw_1.Object.lofc_espcod[il_Fila]
li_Lote			=	dw_1.Object.lofc_lotefc[il_Fila]
li_Secuencia	=	dw_1.Object.lfcd_secuen[il_Fila]

CHOOSE CASE as_Columna
	CASE "lofc_pltcod"
		li_Planta		=	Integer(as_Valor)

	CASE "lofc_espcod"
		li_Especie		=	Integer(as_Valor)

	CASE "lofc_lotefc"
		li_Lote			=	Integer(as_Valor)

	CASE "lfcd_secuen"
		li_Secuencia	=	Integer(as_Valor)

END CHOOSE

IF NOT IsNull(li_Planta) AND NOT IsNull(li_Especie) AND NOT IsNull(li_Lote) AND  &
	NOT IsNull(li_Secuencia) THEN
	IF iuo_LoteFC.Existe(li_Planta,li_Especie,li_Secuencia,li_Lote,True,SqlCa) THEN
		lb_Retorno	=	True
	ELSE
		lb_Retorno	=	False
	END IF
END IF

RETURN lb_Retorno
end function

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
String	ls_Camara, ls_Planta, ls_Especie, ls_Lote, ls_Secuencia

ls_Camara		=	String(dw_1.Object.cama_codigo[il_Fila])
ls_Planta		=	String(dw_1.Object.lofc_pltcod[il_Fila])
ls_Especie		=	String(dw_1.Object.lofc_espcod[il_Fila])
ls_Lote			=	String(dw_1.Object.lofc_lotefc[il_Fila])
ls_Secuencia	=	String(dw_1.Object.lfcd_secuen[il_Fila])

CHOOSE CASE as_Columna
	CASE "cama_codigo"
		ls_Camara		=	as_Valor

	CASE "lofc_pltcod"
		ls_Planta		=	as_Valor

	CASE "lofc_espcod"
		ls_Especie		=	as_Valor

	CASE "lofc_lotefc"
		ls_Lote			=	as_Valor

	CASE "lfcd_secuen"
		ls_Secuencia	=	as_Valor

END CHOOSE

ll_Fila	=	dw_1.Find("cama_codigo = " + ls_Camara + " AND " + &
							 "lofc_pltcod = " + ls_Planta + " AND " + &
							 "lofc_espcod = " + ls_Especie + " AND " + &
							 "lofc_lotefc = " + ls_Lote + " AND " + &
							 "lfcd_secuen = " + ls_Secuencia, &
							 1, dw_1.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	MessageBox("Error", "Registro ya fue ingresada anteriormente", Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine buscaenvase ();str_busqueda	lstr_busq
String			ls_Nula

SetNull(ls_Nula)

lstr_busq.argum[1]	=	String(dw_1.Object.enva_tipoen[il_Fila])

OpenWithParm(w_busc_envases, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	dw_1.SetColumn("enva_codigo")
	dw_1.Object.enva_codigo[il_Fila]	=	Integer(ls_Nula)
	dw_1.Object.enva_nombre[il_Fila]	=	ls_Nula
	dw_1.SetFocus()
ELSE
	dw_1.Object.enva_codigo[il_Fila]	=	Integer(lstr_busq.argum[2])
	dw_1.Object.enva_nombre[il_Fila]	=	lstr_busq.argum[3]
	
	ExisteEnvase(dw_1.object.enva_tipoen[il_fila], &
					dw_1.object.enva_codigo[il_fila], istr_Envase)
END IF

RETURN
end subroutine

public function boolean hayexistencia (string as_columna, string as_valor);Boolean		lb_Retorno = True
Integer		li_Planta, li_Camara, li_PlantaLote, li_EspecieLote, li_NumeroLote, &
				li_Secuencia
Decimal{2}	ld_Cantidad, ld_Saldo

li_Planta		=	dw_1.Object.plde_codigo[il_Fila]
li_Camara		=	dw_1.Object.cama_codigo[il_Fila]
li_PlantaLote	=	dw_1.Object.lofc_pltcod[il_Fila]
li_EspecieLote	=	dw_1.Object.lofc_espcod[il_Fila]
li_NumeroLote	=	dw_1.Object.lofc_lotefc[il_Fila]
li_Secuencia	=	dw_1.Object.lfcd_secuen[il_Fila]
ld_Cantidad		=	dw_1.Object.mfcd_bulent[il_Fila]

CHOOSE CASE as_Columna
		
	CASE "plde_codigo"
		li_Planta		=	Integer(as_Valor)
		
	CASE "cama_codigo"
		li_Camara		=	Integer(as_Valor)
		
	CASE "lofc_pltcod"
		li_PlantaLote	=	Integer(as_Valor)
		
	CASE "lofc_espcod"
		li_EspecieLote	=	Integer(as_Valor)
		
	CASE "lofc_lotefc"
		li_NumeroLote	=	Integer(as_Valor)

	CASE "lfcd_secuen"
		li_Secuencia	=	Integer(as_Valor)

	CASE "mfcd_bulent"
		ld_Cantidad		=	Dec(as_Valor)

END CHOOSE

SELECT	IsNull(Sum(caex_canbul), 0)
	INTO	:ld_Saldo
	FROM	dba.spro_camaraexistecom
	WHERE	plde_codigo	=	:li_Planta
	AND	cama_codigo	=	:li_Camara
	AND	lofc_pltcod	=	:li_PlantaLote
	AND	lofc_espcod	=	:li_EspecieLote
	AND	lofc_lotefc	=	:li_NumeroLote
	AND	lfcd_secuen	=	:li_Secuencia ;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla de Existencia Frigorífico")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "No existe Lote en Cámara especificada.~r~r" + &
					"Ingrese o seleccione otros antecedentes.")
	
	lb_Retorno	=	False
ELSEIF Not IsNull(ld_Cantidad) AND ld_Cantidad > 0 AND ld_Cantidad > ld_Saldo THEN
	MessageBox("Atención", "No hay Existencia suficiente del Lote~r" + &
					"en Cámara especificada.~r~r" + &
					"Ingrese o seleccione otros antecedentes.")
	
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public subroutine buscalote ();str_busqueda	lstr_busq
String			ls_Nula

SetNull(ls_Nula)

//IF istr_mant.Argumento[2]='21' THEN
//	lstr_busq.argum[3]	=	"21"
//   lstr_busq.argum[4]	=	istr_mant.argumento[9]
//	lstr_busq.argum[5]   =  istr_mant.argumento[6]
//	lstr_busq.argum[6]   =  istr_mant.argumento[8]
//END IF

IF istr_Mant.Argumento[4]	=	'1' THEN
	lstr_Busq.Argum[1]	=	String(dw_1.Object.lofc_pltcod[il_Fila])
	lstr_Busq.Argum[2]	=	String(dw_1.Object.lofc_espcod[il_Fila])
	
	OpenWithParm(w_busc_lotesfrutacomdeta_recepcion, lstr_busq)
ELSE
	lstr_Busq.Argum[1]	=	String(dw_1.Object.lofc_pltcod[il_Fila])
	lstr_Busq.Argum[2]	=	String(dw_1.Object.cama_codigo[il_Fila])
	lstr_Busq.Argum[3]	=	String(dw_1.Object.lofc_espcod[il_Fila])
	
	OpenWithParm(w_busc_lotesfrutacomdeta_despacho, lstr_busq)
END IF

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[4] = "" THEN
	dw_1.SetColumn("lofc_lotefc")
//	IF istr_mant.Argumento[2]<>'21' THEN
//		dw_1.Object.vari_nombre[il_Fila] = ls_Nula
//		dw_1.Object.prod_nombre[il_Fila] = ls_Nula
//	END IF
	dw_1.SetFocus()
ELSEIF Duplicado("lofc_lotefc", lstr_busq.argum[3]) THEN
	dw_1.SetItem(il_Fila, "lofc_lotefc", Integer(ls_Nula))
ELSE
	dw_1.Object.lofc_lotefc[il_Fila] =	Integer(lstr_busq.argum[3])
	dw_1.Object.lfcd_secuen[il_Fila] =	Integer(lstr_Busq.Argum[4])
	dw_1.Object.vari_nombre[il_Fila] =	lstr_busq.argum[6]
	dw_1.Object.prod_nombre[il_Fila] =	lstr_busq.argum[8]

	iuo_LoteFC.Existe(iuo_PltaLote.Codigo, iuo_Especie.Codigo, &
							Integer(lstr_Busq.Argum[4]),Integer(lstr_busq.argum[3]), &
							False, sqlca)

	IF istr_Mant.Argumento[4]	=	'2' THEN
		dw_1.Object.cama_codigo[il_Fila] =	Integer(lstr_Busq.Argum[14])
		dw_1.Object.mfcd_bulent[il_Fila] =	Dec(lstr_busq.argum[13])
		dw_1.Object.mfcd_kgnent[il_Fila]	=	Dec(lstr_busq.argum[13]) * iuo_LoteFC.KilosPromed
	END IF
END IF

RETURN
end subroutine

on w_mant_deta_movtotraspasointerno.create
call super::create
end on

on w_mant_deta_movtotraspasointerno.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;///Agrega L.M.
//IF istr_mant.Argumento[2]='21' THEN
// dw_1.Object.lote_espcod[il_Fila]				=	integer(istr_mant.Argumento[8])
//	dw_1.Object.lote_espcod.Protect 				=	1
//	dw_1.Object.lote_espcod.BackGround.Color 	=	RGB(166,180,210)
//	dw_1.Object.prod_nombre[il_fila]				=	istr_mant.argumento[7]
//	dw_1.Object.prod_nombre.Protect 				=	1
//	dw_1.Object.prod_nombre.BackGround.Color 	=	RGB(166,180,210)
//	dw_1.Object.vari_nombre[il_fila]				=	istr_mant.argumento[10]
//	dw_1.Object.vari_nombre.Protect 				=	1
//	dw_1.Object.vari_nombre.BackGround.Color 	=	RGB(166,180,210)
//END IF

ias_campo[1]	=	String(dw_1.Object.plde_coorde[il_Fila])
ias_campo[2]	=	String(dw_1.Object.cama_codigo[il_Fila])
ias_campo[3]	=	String(dw_1.Object.lofc_pltcod[il_Fila])
ias_campo[4]	=	String(dw_1.Object.lofc_espcod[il_Fila])
ias_campo[5]	=	String(dw_1.Object.lofc_lotefc[il_Fila])
ias_campo[6]	=	String(dw_1.Object.lfcd_secuen[il_Fila])
ias_campo[7]	=	String(dw_1.Object.mfcd_bulent[il_Fila])
ias_campo[8]	=	String(dw_1.Object.mfcd_kgnent[il_Fila])



IF istr_mant.Agrega THEN
	dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
	dw_1.Object.plde_coorde[il_Fila]	=	Integer(istr_Mant.Argumento[5])
	dw_1.Object.tpmv_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[2])
	dw_1.Object.mfco_numero[il_Fila]	=	Integer(istr_Mant.Argumento[3])
	dw_1.Object.lofc_pltcod[il_fila]	=	gstr_ParamPlanta.CodigoPlanta
	dw_1.Object.lofc_espcod[il_fila]	=	gstr_ParamPlanta.CodigoEspecie

	iuo_PltaLote.Existe(gstr_ParamPlanta.CodigoPlanta, True, sqlca)
	iuo_Especie.Existe(gstr_ParamPlanta.CodigoEspecie, True, sqlca)
ELSEIF Not istr_mant.Agrega AND Not istr_mant.Borra THEN
	iuo_PltaLote.Existe(dw_1.Object.lofc_pltcod[il_fila], True, sqlca)
	iuo_Especie.Existe(dw_1.Object.lofc_espcod[il_fila], True, sqlca)
	iuo_LoteFC.Existe(iuo_PltaLote.Codigo, iuo_Especie.Codigo, &
							dw_1.Object.lfcd_secuen[il_fila], &
							dw_1.Object.lofc_lotefc[il_fila], True, sqlca)
	ExisteVariedad(iuo_Especie.Codigo, iuo_LoteFC.Variedad, istr_Variedad)
	iuo_Productor.Existe(iuo_LoteFC.Productor, True, sqlca)
END IF
end event

event ue_deshace();call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.Object.plde_coorde[il_fila]	=	Integer(ias_campo[1])
	dw_1.Object.cama_codigo[il_fila]	=	Integer(ias_campo[2])
	dw_1.Object.lofc_pltcod[il_fila]	=	Integer(ias_campo[3])
	dw_1.Object.lofc_espcod[il_fila]	=	Integer(ias_campo[4])
	dw_1.Object.lofc_lotefc[il_fila]	=	Integer(ias_campo[5])
	dw_1.Object.lfcd_secuen[il_fila]	=	Integer(ias_campo[6])
	dw_1.Object.mfcd_bulent[il_fila]	=	Dec(ias_campo[7])
	dw_1.Object.mfcd_kgnent[il_fila]	=	Dec(ias_campo[8])
END IF
end event

event ue_antesguardar();Integer	li_Contador, li_row, li_suma
String	ls_Mensaje, ls_Columna[]

IF IsNull(dw_1.Object.cama_codigo[il_Fila]) THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nNúmero de Cámara"
	ls_Columna[li_Contador]	=	"cama_codigo"
END IF

IF IsNull(dw_1.Object.lofc_pltcod[il_Fila]) OR &
	dw_1.Object.lofc_pltcod[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nPlanta de Lote"
	ls_Columna[li_Contador]	=	"lofc_pltcod"
END IF

IF IsNull(dw_1.Object.lofc_espcod[il_Fila]) OR &
	dw_1.Object.lofc_espcod[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nEspecie"
	ls_Columna[li_Contador]	=	"lofc_espcod"
END IF

IF IsNull(dw_1.Object.lofc_lotefc[il_Fila]) OR &
	dw_1.Object.lofc_lotefc[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nNúmero de Lote"
	ls_Columna[li_Contador]	=	"lofc_lotefc"
END IF

IF IsNull(dw_1.Object.lfcd_secuen[il_Fila]) OR &
	dw_1.Object.lfcd_secuen[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nSecuencia del Lote"
	ls_Columna[li_Contador]	=	"lfcd_secuen"
END IF

IF IsNull(dw_1.Object.mfcd_bulent[il_Fila]) OR &
	dw_1.Object.mfcd_bulent[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCantidad de Bultos"
	ls_Columna[li_Contador]	=	"mfcd_bulent"
END IF

IF li_Contador > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_Columna[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo();call super::ue_nuevo;///Agrega L.M.
//IF istr_mant.Argumento[2]='21' THEN
//   dw_1.Object.lote_espcod[il_Fila]				=	integer(istr_mant.Argumento[8])
//	dw_1.Object.prod_nombre[il_fila]				=	istr_mant.argumento[7]
//	dw_1.Object.vari_nombre[il_fila]				=	istr_mant.argumento[10]
//ELSE
//   dw_1.Object.lote_espcod[il_Fila]	=	gstr_ParamPlanta.CodigoEspecie
//	iuo_Especie.Existe(gstr_ParamPlanta.CodigoEspecie, True, sqlca)
//END IF

dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
dw_1.Object.plde_coorde[il_Fila]	=	Integer(istr_Mant.Argumento[5])
dw_1.Object.tpmv_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[2])
dw_1.Object.mfco_numero[il_Fila]	=	Integer(istr_Mant.Argumento[3])
dw_1.Object.lofc_pltcod[il_Fila]	=	gstr_ParamPlanta.CodigoPlanta
dw_1.Object.lofc_pltcod[il_Fila]	=	gstr_ParamPlanta.CodigoEspecie

iuo_PltaLote.Existe(gstr_ParamPlanta.CodigoPlanta, True, sqlca)
end event

event open;/* 
	Argumentos
		istr_Mant.Argumento[1]	=	Código Planta
		istr_Mant.Argumento[2]	=	Tipo de Movimiento
		istr_Mant.Argumento[3]	=	Número de Movimiento
		istr_Mant.Argumento[4]	=	Sentido del Movimiento
		istr_Mant.Argumento[5]	=	Código Planta Dest.
											1 => Recepcion, 2 => Despacho
*/

x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

iuo_PltaLote	=	Create uo_plantadesp
iuo_Especie		=	Create uo_especie
iuo_LoteFC		=	Create uo_lotesfrutacomer
iuo_Productor	=	Create uo_productores
iuo_Camara		=	Create uo_camarasfrigo
iuo_TipoMovto	=	Create uo_tipomovtofruta

dw_1.GetChild("plde_codigo", idwc_Planta)
idwc_Planta.SetTransObject(sqlca)
idwc_Planta.Retrieve()

dw_1.GetChild("cama_codigo", idwc_Camara)
idwc_Camara.SetTransObject(sqlca)
idwc_Camara.Retrieve(Integer(istr_Mant.Argumento[1]))
idwc_Camara.SetSort("cama_nombre A")
idwc_Camara.Sort()

dw_1.GetChild("lote_pltcod", idwc_Planta)
idwc_Planta.SetTransObject(sqlca)
idwc_Planta.Retrieve()
idwc_Planta.SetSort("plde_nombre A")
idwc_Planta.Sort()

dw_1.GetChild("lote_espcod", idwc_Especie)
idwc_Especie.SetTransObject(sqlca)
idwc_Especie.Retrieve()
idwc_Especie.SetSort("espe_nombre A")
idwc_Especie.Sort()

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)

iuo_TipoMovto.Existe(Integer(istr_Mant.Argumento[2]),True,SQLCA)

IF iuo_TipoMovto.Sentido = 2 THEN
	dw_1.Modify("titulo.Text = 'DETALLE DE DESPACHO'")
ELSE
	dw_1.Modify("titulo.Text = 'DETALLE DE RECEPCION'")
END IF
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_movtotraspasointerno
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_movtotraspasointerno
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_movtotraspasointerno
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_movtotraspasointerno
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_movtotraspasointerno
integer x = 2354
integer y = 308
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_movtotraspasointerno
integer x = 2354
integer y = 132
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_movtotraspasointerno
integer x = 2354
integer y = 480
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_movtotraspasointerno
integer width = 2153
integer height = 1572
string dataobject = "dw_mant_movtofrutatraspaso"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna, ls_Nula
Integer	li_Secuencia

SetNull(ls_Nula)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna
	CASE "cama_codigo"
		IF Not iuo_Camara.Existe(Integer(istr_Mant.Argumento[5]), &
						Integer(Data), True, sqlca) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		ELSEIF Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		ELSE
			IF iuo_TipoMovto.Sentido = 2 THEN
				IF Not HayExistencia(ls_Columna, Data) THEN
					This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
					
					RETURN 1
				END IF
			END IF
		END IF

	CASE "lofc_pltcod"
		IF Not iuo_PltaLote.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		ELSEIF Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		ELSE
			IF iuo_TipoMovto.Sentido = 2 THEN
				IF Not HayExistencia(ls_Columna, Data) THEN
					This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
					
					RETURN 1
				END IF
			END IF
		END IF

	CASE "lofc_espcod"
		IF Not iuo_Especie.Existe(Integer(Data), True, sqlca) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		ELSEIF Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		ELSE
			IF iuo_TipoMovto.Sentido = 2 THEN
				IF Not HayExistencia(ls_Columna, Data) THEN
					This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
					
					RETURN 1
				END IF
			END IF
		END IF

	CASE "lofc_lotefc"
		IF Not iuo_LoteFC.ExisteEncab(iuo_PltaLote.Codigo, iuo_Especie.Codigo, & 
												Integer(Data), True, SqlCa) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))

			RETURN 1
		ELSEIF Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
			
			RETURN 1
		ELSE
			IF iuo_TipoMovto.Sentido = 2 THEN
				IF Not HayExistencia(ls_Columna, Data) THEN
					This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
					
					
					RETURN 1
				END IF
			END IF
		END IF

	CASE "lfcd_secuen"
		IF Not ExisteLote(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))

			RETURN 1
		ELSEIF Duplicado(ls_Columna, Data) THEN
			This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))

			RETURN 1
		ELSE
			iuo_Productor.Existe(iuo_LoteFC.Productor, True, sqlca)
			ExisteVariedad(iuo_Especie.Codigo,iuo_LoteFC.Variedad,istr_variedad)

			This.SetItem(il_Fila, "lfcd_secuen", iuo_LoteFC.Secuencia)
			This.SetItem(il_Fila, "vari_nombre", istr_Variedad.Nombre)
			This.SetItem(il_Fila, "prod_nombre", iuo_Productor.Nombre)

			IF iuo_TipoMovto.Sentido = 2 THEN
				IF Not HayExistencia(ls_Columna, Data) THEN
					This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
					
					RETURN 1
				END IF
			END IF
		END IF

	CASE "mfcd_bulent"
		IF iuo_TipoMovto.Sentido = 2 THEN
			IF Not HayExistencia(ls_Columna, Data) THEN
				This.SetItem(il_Fila, ls_Columna, Integer(ls_Nula))
				
				RETURN 1
			END IF
		END IF

		This.Object.mfcd_kgnent[il_Fila]	=	Dec(Data) * iuo_LoteFC.KilosPromed

END CHOOSE
end event

event dw_1::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name
	CASE "b_buscalotes"
		BuscaLote()

END CHOOSE
end event

