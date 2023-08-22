$PBExportHeader$w_mant_deta_movtofrutcomer_despachoventa.srw
forward
global type w_mant_deta_movtofrutcomer_despachoventa from w_mant_detalle_csd
end type
type ole_puerta from olecustomcontrol within w_mant_deta_movtofrutcomer_despachoventa
end type
type str_pesaje from structure within w_mant_deta_movtofrutcomer_despachoventa
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

global type w_mant_deta_movtofrutcomer_despachoventa from w_mant_detalle_csd
integer width = 2491
integer height = 1744
string title = "Detalle de Despacho Ventas"
ole_puerta ole_puerta
end type
global w_mant_deta_movtofrutcomer_despachoventa w_mant_deta_movtofrutcomer_despachoventa

type variables
uo_plantadesp			iuo_PltaLote
uo_especie				iuo_Especie
uo_lotesfrutacomer	iuo_Lote
uo_productores			iuo_Productor
uo_camarasbode			iuo_Camara
uo_tipomovtofruta		iuo_TipoMovto

str_variedad				istr_Variedad
str_envase				istr_Envase

DataWindowChild		idwc_Camara, idwc_Planta, idwc_Especie
DataWindowChild		idwc_tipoenvase

str_puertacomm			istr_puertacomm

String						is_columna, is_columna_anterior, is_calidad
Boolean					ib_flag, ib_control
Decimal{9}				id_pesnet1, id_pesnet2, id_pesnet3, id_lfcd_kilobp
Integer					ii_cantidad, ii_cantori

Private:
str_pesaje				wstr_pesaje
end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
public function boolean wf_hayexistencia (string as_columna, string as_valor, decimal ad_cantidad)
public function boolean wf_existeenorden (integer ai_tipord, long al_orden, integer ai_especie)
public function boolean wf_precio_unitario (date ad_fecha, integer ai_especie, integer ai_variedad, string as_calibre)
public function boolean wf_cargadatosecuencia (integer ai_secuencia)
public subroutine wf_calcula_neto (integer ai_sentido)
public subroutine wf_calculavalores ()
public function boolean wf_existeromana (integer ai_planta)
public function boolean wf_bultosexistencia (integer ai_cantidad)
public subroutine wf_buscalote ()
public subroutine wf_buscalote2 ()
public subroutine wf_buscaenvase ()
public function boolean wf_cargatarja (long al_tarja, integer al_cliente, integer ai_planta)
end prototypes

public function boolean duplicado (string as_columna, string as_valor);Long		ll_Fila
String	ls_Camara, ls_Planta, ls_Especie, ls_Lote, ls_TiEnvase, ls_Envase, ls_secuen

ls_Camara	=	String(dw_1.Object.cama_codigo[il_Fila])
ls_Planta	=	String(dw_1.Object.lofc_pltcod[il_Fila])
ls_Especie	=	String(dw_1.Object.lofc_espcod[il_Fila])
ls_Lote		=	String(dw_1.Object.lofc_lotefc[il_Fila])
ls_TiEnvase	=	String(dw_1.Object.enva_tipoen[il_Fila])
ls_Envase	=	String(dw_1.Object.enva_codigo[il_Fila])
ls_secuen   = 	String(dw_1.Object.lfcd_secuen[il_Fila])

CHOOSE CASE as_Columna
	CASE "cama_codigo"
		ls_Camara	=	as_Valor

	CASE "lofc_pltcod"
		ls_Planta	=	as_Valor

	CASE "lofc_espcod"
		ls_Especie	=	as_Valor

	CASE "lofc_lotefc"
		ls_Lote		=	as_Valor

	CASE "enva_tipoen"
		ls_TiEnvase	=	as_Valor

	CASE "enva_codigo"
		ls_Envase	=	as_Valor
	
	CASE "lfcd_secuen"
		ls_secuen	=	as_Valor

END CHOOSE

ll_Fila	=	dw_1.Find("cama_codigo = " + ls_Camara + " AND " + &
							"lofc_pltcod = " + ls_Planta + " AND " + &
							"lofc_espcod = " + ls_Especie + " AND " + &
							"lofc_lotefc = " + ls_Lote + " AND " + &
							"enva_tipoen = " + ls_TiEnvase + " AND " + &
							"enva_codigo = " + ls_Envase + " AND " + &
							"lfcd_secuen = " + ls_Secuen ,1, dw_1.RowCount())

IF ll_Fila > 0 AND ll_Fila <> il_Fila THEN
	MessageBox("Error", "Registro ya fue ingresada anteriormente", Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean wf_hayexistencia (string as_columna, string as_valor, decimal ad_cantidad);Boolean	lb_Retorno = True
Integer	li_Planta, li_Camara, li_PlantaLote, li_EspecieLote, li_NumeroLote, &
			li_secuen
Decimal{3} ld_Saldo

li_Planta					=	dw_1.Object.plde_codigo[il_Fila]
li_Camara				=	dw_1.Object.cama_codigo[il_Fila]
li_PlantaLote			=	dw_1.Object.lofc_pltcod[il_Fila]
li_EspecieLote			=	dw_1.Object.lofc_espcod[il_Fila]
li_NumeroLote			=	dw_1.Object.lofc_lotefc[il_Fila]

If is_columna		=	"fgmb_nrotar" Then
	li_secuen				=	dw_1.Object.lfcd_secuen[il_Fila]
ElseIf is_columna	=	"tarja2" Then
	li_secuen				=	dw_1.Object.lfcd_secue2[il_Fila]
ElseIf is_columna	=	"tarja3" Then
	li_secuen				=	dw_1.Object.lfcd_secue3[il_Fila]
End If

CHOOSE CASE as_Columna
		
	CASE "plde_codigo"
		li_Planta			=	Integer(as_Valor)
		
	CASE "cama_codigo"
		li_Camara		=	Integer(as_Valor)
		
	CASE "lofc_pltcod"
		li_PlantaLote	=	Integer(as_Valor)
		
	CASE "lofc_espcod"
		li_EspecieLote	=	Integer(as_Valor)
		
	CASE "lofc_lotefc"
		li_NumeroLote	=	Integer(as_Valor)
		
	CASE "lfcd_secuen"
		li_Secuen		=	Integer(as_Valor)
		
End CHOOSE

If li_Planta > 0 AND li_Camara > 0 AND Li_PlantaLote > 0 AND li_EspecieLote  > 0 AND li_NumeroLote  > 0 Then

	SELECT	IsNull(Sum(caex_canbul), 0)
		INTO	:ld_Saldo
		FROM	dbo.spro_camaraexistecom
		WHERE plde_codigo	=	:li_Planta
		AND   cama_codigo	=	:li_Camara
		AND   lofc_pltcod	=	:li_PlantaLote
		AND   lofc_espcod	=	:li_EspecieLote
		AND   lofc_lotefc	=	:li_NumeroLote
		AND   lfcd_secuen	=	:li_secuen;
	
	If sqlca.SQLCode = -1 Then
		F_ErrorBaseDatos(sqlca, "Lectura tabla de Existencia Camara Comercial")
		
		lb_Retorno	=	False
	ElseIf sqlca.SQLCode = 100 Then
		MessageBox("Atención", "No existe Lote en Cámara especIficada.~r~r" + &
						"Ingrese o seleccione otros antecedentes.")
		
		lb_Retorno	=	False
	ElseIf IsNull(ad_Cantidad) OR ad_Cantidad < 0 Then//AND ad_Cantidad > ld_Saldo Then
		MessageBox("Atención", "No hay Existencia suficiente del Lote~r" + &
						"en Cámara especIficada.~r~r" + &
						"Ingrese o seleccione otros antecedentes.")
		lb_Retorno	=	False
	End If
End If
	
RETURN lb_Retorno
end function

public function boolean wf_existeenorden (integer ai_tipord, long al_orden, integer ai_especie);Boolean lb_Retorno = TRUE
Integer li_cuenta, li_Planta

//li_Planta = dw_1.Object.plde_codigo[il_fila]
//
//IF integer(istr_mant.argumento[2]) = 32 THEN
//	SELECT	count(espe_codigo)
//		INTO	:li_cuenta
//		FROM	dbo.spro_ordenretiroventadet
//		WHERE	plde_codigo	=	:li_Planta
//		AND	tdop_codigo	=	:ai_tipord
//		AND 	oret_numero	=	:al_orden
//		AND   espe_codigo =  :ai_especie;
//	
//	IF sqlca.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlca, "Lectura tabla de Orden de Retiro")
//		
//		lb_Retorno	=	False
//	ELSEIF sqlca.SQLCode = 100 THEN
//		MessageBox("Atención", "No existe Especie en Orden de Retiro.~r~r" + &
//						"Ingrese o seleccione otra.")
//		
//		lb_Retorno	=	False
//	END IF
//	
//	IF li_cuenta = 0 THEN
//		MessageBox("Atención", "No existe la Especie en la Orden de Retiro Seleccionada.~r~r" + &
//						"Ingrese o seleccione otra.")
//		
//		lb_Retorno	=	False
//	END IF
//ELSE
//	SELECT	count(espe_codigo)
//		INTO	:li_cuenta
//		FROM	dbo.spro_ordenventacomdeta
//		WHERE	plde_codigo	=	:li_Planta
//		AND	odfc_numero	=	:al_orden
//		AND   espe_codigo =  :ai_especie;
//	
//	IF sqlca.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlca, "Lectura tabla de Orden de Venta")
//		
//		lb_Retorno	=	False
//	ELSEIF sqlca.SQLCode = 100 THEN
//		MessageBox("Atención", "No existe Especie en Orden de Venta.~r~r" + &
//						"Ingrese o seleccione otra.")
//		
//		lb_Retorno	=	False
//	END IF
//	
//	IF li_cuenta = 0 THEN
//		MessageBox("Atención", "No existe la Especie en la Orden de Venta Seleccionada.~r~r" + &
//						"Ingrese o seleccione otra.")
//		
//		lb_Retorno	=	False
//	END IF
//
//END IF

lb_Retorno	=	TRUE

RETURN lb_Retorno
end function

public function boolean wf_precio_unitario (date ad_fecha, integer ai_especie, integer ai_variedad, string as_calibre);Decimal 	ld_unitario, ld_bruto, ld_nula
Date		ldt_fecha

SetNull(ld_nula)
SELECT Max(tafc_fecham)
    INTO :ldt_fecha
    FROM dbo.spro_tarifafrutacomercial  
   WHERE tafc_fecham	<=	:ad_fecha
	   AND tafc_fechat		>=	:ad_fecha
        AND espe_codigo 	= 		:ai_especie
        AND vari_codigo 	= 		:ai_variedad
        AND refe_gcalib 		=		:as_calibre;
		  
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_tarifafrutacomercial")
	RETURN True
ELSEIF sqlca.sqlcode	=	0 THEN
	
	SELECT tafc_preuni, tafc_prebru
		 INTO :ld_unitario, :ld_bruto
		 FROM dbo.spro_tarifafrutacomercial  
		WHERE :ad_fecha between tafc_fecham and tafc_fechat
			AND tafc_fecham	=	:ldt_fecha
			AND espe_codigo	= :ai_especie
			AND vari_codigo 	= :ai_variedad
			AND refe_gcalib 	= :as_calibre;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla spro_tarifafrutacomercial")
		RETURN True
	ELSEIF sqlca.sqlcode	=	0 THEN
		dw_1.SetItem(il_fila,"mfcd_preuni",ld_unitario)
		dw_1.SetItem(il_fila,"mfcd_prebru",ld_bruto)
		RETURN True
	ELSE	
		dw_1.SetItem(il_fila,"mfcd_preuni",ld_nula)
		RETURN False
	END IF
ELSE	
	dw_1.SetItem(il_fila,"mfcd_preuni",ld_nula)
	RETURN False
END IF
end function

public function boolean wf_cargadatosecuencia (integer ai_secuencia);Integer			li_planta, li_especie, li_lote, li_secuencia, li_variedad, li_productor, &
					li_tipoenvase, li_envase, li_bultos, li_camara, li_control, li_bultosex, li_estado
Long				ll_productor, ll_filas, secue1, secue2, secue3
Dec{3}			ldc_neto
String			ls_envanombre, ls_varinombre, ls_prodnombre, ls_Calibre

uo_variedades	luo_variedades
uo_productores	luo_productores
uo_envases		luo_envases

IF dw_1.Find("lfcd_secuen = " + String(ai_secuencia) , 1, dw_1.RowCount()) > 0 THEN
	MessageBox("Atención", "La Secuencia ya ha sido ingresada .~r~r" + &
					"Ingrese o seleccione otra.", StopSign!)
	Return False
END IF

li_planta	=	dw_1.Object.lofc_pltcod[il_fila]
li_especie	=	dw_1.Object.lofc_espcod[il_fila]
li_lote		=	dw_1.Object.lofc_lotefc[il_fila]

Select lofc_pltcod, lofc_espcod,	lofc_lotefc, lfcd_secuen, 	vari_codigo,
		 prod_codigo, enva_tipoen,	enva_codigo, lfcd_bultos,	lfcd_kilnet,
		 refe_gcalib, cale_calida, lfcd_kilobp
Into 	:li_planta,   :li_especie,   :li_lote,  :li_secuencia,:li_variedad, 
		:ll_productor,:li_tipoenvase,:li_envase,:li_bultos, 	:ldc_neto,
		:ls_Calibre,  :is_calidad, :id_lfcd_kilobp
From dbo.spro_lotesfrutacomdeta
where lofc_pltcod	=:li_planta
  and lofc_espcod	=:li_especie
  and lofc_lotefc	=:li_lote
  and lfcd_secuen	=:ai_secuencia;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla de Detalle de Lotes Comerciales")
	Return False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "No existe la Tarja ingresada .~r~r" + &
					"Ingrese o seleccione otra.")
	Return False
END IF

ii_cantori	=	li_bultos

IF li_bultos = 1 THEN
	ib_control 											= 	True
	
	dw_1.Object.mfcd_bulent.Protect				=	1
	dw_1.Object.mfcd_bulent.BackGround.Color	=	rgb(166,180,210)
ELSE
	ib_control											=	False
	ldc_neto												=	ldc_neto / li_bultos
	dw_1.Object.mfcd_bulent[il_fila]				=	li_bultos
	dw_1.Object.nrotar2[il_fila]						=	li_bultos

	dw_1.Object.mfcd_bulent.Protect				=	0
	dw_1.Object.mfcd_bulent.BackGround.Color	=	RGB(255,255,255)
END IF

luo_envases	=	Create uo_envases
IF Not luo_envases.Existe(li_tipoenvase,li_envase, True, Sqlca) THEN
	Return False
END IF

luo_variedades 	= 	Create uo_variedades
IF NOT luo_variedades.Existe(li_especie, li_variedad, true, sqlca) THEN
	Return False
END IF

luo_productores	=	Create uo_productores
IF NOT luo_productores.Existe(ll_productor, true, sqlca) THEN
	Return False
END IF

Select cama_codigo, caex_canbul
Into :li_camara, :li_bultosex
From dbo.spro_camaraexistecom
where plde_codigo	=  :li_planta
  and lofc_pltcod =	:li_planta
  and lofc_espcod	=	:li_especie
  and lofc_lotefc	=	:li_lote
  and lfcd_secuen	=	:li_secuencia;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla de Movimiento de Existencia")
	Return False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "No existe Tarja en existencias.~r~r" + &
					"Ingrese o seleccione otra.")
	Return False
ELSEIF li_bultosex < 1 THEN
	MessageBox("Atención", "No hay datos en existencia para esta Tarja.~r~r" + &
					"Ingrese o seleccione otra.")
	Return False
END IF

dw_1.Object.cama_codigo[il_fila]	=	li_camara
dw_1.Object.lofc_pltcod[il_fila]	=	li_planta
dw_1.Object.lofc_espcod[il_fila]	=	li_especie
dw_1.Object.lofc_lotefc[il_fila]	=	li_lote
dw_1.Object.vari_nombre[il_fila]	=	luo_variedades.NombreVariedad
dw_1.Object.prod_nombre[il_fila]	=	luo_productores.Nombre
dw_1.Object.enva_tipoen[il_fila]	=	li_tipoenvase
dw_1.Object.enva_codigo[il_fila]	=	li_envase
dw_1.Object.enva_nombre[il_fila]	=	luo_envases.Nombre
dw_1.Object.vari_codigo[il_fila] =  li_variedad
dw_1.Object.mfcd_calibr[il_fila]	=  ls_Calibre
dw_1.Object.lfcd_secuen[il_fila]	=	li_secuencia
dw_1.Object.cale_calida[il_fila]	=	is_calidad

id_pesnet1								=	ldc_neto

IF ib_control THEN 
	dw_1.Object.mfcd_bulent[il_fila]	=	li_bultos
ELSE
	dw_1.Object.mfcd_bulent[il_fila]	=	li_bultosex
	ii_cantidad								=	li_bultosex
END IF


wf_Precio_Unitario(Date(istr_mant.argumento[13]),li_especie,li_variedad,ls_calibre)


Destroy luo_envases;
Destroy luo_variedades;
Destroy luo_productores;

dw_1.AcceptText()

iuo_Lote.Existe(dw_1.Object.lofc_pltcod[il_fila], dw_1.Object.lofc_espcod[il_fila],&
					dw_1.Object.lofc_lotefc[il_fila], dw_1.Object.lfcd_secuen[il_fila],True, sqlca)
					
IF NOT wf_HayExistencia("", "", li_bultos) THEN
	SetNull(li_bultos)
	
	dw_1.Object.cama_codigo[il_fila]		=	li_bultos
	dw_1.Object.lofc_pltcod[il_fila]		=	li_bultos
	dw_1.Object.lofc_espcod[il_fila]		=	li_bultos
	dw_1.Object.lofc_lotefc[il_fila]		=	li_bultos
	
	IF is_columna	=	"fgmb_nrotar" THEN
		dw_1.Object.lfcd_secuen[il_fila]	=	li_bultos
	ELSEIF is_columna	=	"tarja2" THEN
		dw_1.Object.lfcd_secue2[il_fila]	=	li_bultos
	ELSEIF is_columna	=	"tarja3" THEN
		dw_1.Object.lfcd_secue3[il_fila]	=	li_bultos
	END IF

	dw_1.Object.vari_nombre[il_fila]		=	""
	dw_1.Object.prod_nombre[il_fila]		=	""
	dw_1.Object.enva_tipoen[il_fila]		=	li_bultos
	dw_1.Object.enva_codigo[il_fila]		=	li_bultos
	dw_1.Object.enva_nombre[il_fila]		=	""
	dw_1.Object.mfcd_bulent[il_fila]		=	li_bultos
	dw_1.Object.mfcd_kgnent[il_fila]		=	Dec(li_bultos)
	dw_1.Object.vari_codigo[il_fila] 		=  li_bultos
	dw_1.Object.mfcd_calibr[il_fila] 		=	""
	 dw_1.Object.cale_calida[il_fila]		=	is_calidad
	dw_1.SetColumn(is_columna)
	dw_1.SetFocus()
	Return False
ELSE
	Return True
END IF

end function

public subroutine wf_calcula_neto (integer ai_sentido);Integer 	li_TipEnv, li_CodEnv, li_Cant, li_Bultos
Dec{3}	ldc_KilosTara, ldc_KilosRomana, ldc_KilosNetos

dw_1.AcceptText()

IF ib_control THEN
	IF dw_1.Object.lfcd_secuen[il_fila] > 0 THEN li_Bultos ++
	IF dw_1.Object.lfcd_secue2[il_fila] > 0 THEN li_Bultos ++
	IF dw_1.Object.lfcd_secue3[il_fila] > 0 THEN li_Bultos ++
	
	CHOOSE CASE li_bultos
		CASE 1
			id_pesnet2 = 0
			id_pesnet3 = 0
		CASE 2
			id_pesnet3 = 0
		END CHOOSE
	
	ldc_KilosNetos 						= 	id_pesnet1 + id_pesnet2 + id_pesnet3
	
	dw_1.Object.mfcd_bulent[il_fila] = 	li_bultos
	
	li_Cant									=	dw_1.Object.mfcd_bulent[il_fila]
ELSE
	IF ii_cantidad * id_pesnet1 > 0 THEN
		ldc_KilosNetos							=	ii_cantidad * id_pesnet1
		dw_1.Object.mfcd_kgnent[il_fila]	=	ldc_KilosNetos
		li_Cant									=	ii_cantidad
	ELSE
		ldc_KilosNetos							=	dw_1.Object.mfcd_kgnent[il_fila]
		li_Cant									=	dw_1.Object.mfcd_bulent[il_fila]
	END IF
END IF

li_TipEnv 	= 	dw_1.Object.enva_tipoen[il_fila]
li_CodEnv 	= 	dw_1.Object.enva_codigo[il_fila]

If IsNull(is_calidad) OR is_calidad = '' THEN
	is_calidad	=	dw_1.Object.cale_calida[il_fila]
END IF

IF IsNull(li_TipEnv * li_CodEnv * li_Cant) THEN Return
	
Select cale_pesoen
Into :ldc_KilosTara
From dbo.spro_calicosechero
Where enva_codigo 	= 	:li_CodEnv
	and enva_tipoen 	= 	:li_TipEnv
	and cale_calida	=	:is_calidad;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla de Calidad de envases cosecheros")
	Return
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "No existe Calidad de Envase .~r~r" + &
					"Ingrese o seleccione otra.")
	Return
END IF

IF ai_sentido = 1 THEN
	ldc_KilosRomana 	= dw_1.Object.romana[il_fila]
	
	IF ldc_KilosRomana <= 0 OR IsNull(ldc_KilosRomana) THEN
		
		ldc_KilosRomana = ldc_KilosNetos + (li_Cant * ldc_KilosTara) + id_lfcd_kilobp
		dw_1.SetItem(il_fila, "mfcd_kilrom", ldc_KilosRomana)
		
	END IF
	
	IF IsNull(id_lfcd_kilobp) THEN
		id_lfcd_kilobp	=	0
	END IF
	
	dw_1.SetItem(il_fila, "mfcd_kgnent", ldc_KilosRomana - (li_Cant * ldc_KilosTara) - id_lfcd_kilobp)
	dw_1.AcceptText()
ELSE
	ldc_KilosNetos		= dw_1.Object.mfcd_kgnent[il_fila]
	
	IF dw_1.Object.romana[il_fila] <= 0 OR IsNull(dw_1.Object.romana[il_fila]) THEN
		ldc_KilosNetos = ldc_KilosNetos
		dw_1.SetItem(il_fila, "mfcd_kgnent", ldc_KilosNetos)
	END IF
	
	dw_1.SetItem(il_fila, "mfcd_kilrom", ldc_KilosNetos + (li_Cant * ldc_KilosTara) + id_lfcd_kilobp)
	dw_1.AcceptText()
END IF
end subroutine

public subroutine wf_calculavalores ();Decimal	ld_valor_bruto, ld_valor_neto

dw_1.AcceptText()

IF il_fila = 0 THEN Return

IF istr_Mant.Argumento[12] = '0' THEN
	dw_1.SetItem(il_fila, "mfcd_prebru", dw_1.Object.mfcd_preuni[il_fila] * (1 + gstr_ParamPlanta.PorcentajeIVA))
ELSE
	dw_1.SetItem(il_fila, "mfcd_preuni", Round(dw_1.Object.mfcd_prebru[il_fila] / (1 + gstr_ParamPlanta.PorcentajeIVA),2))
END IF
end subroutine

public function boolean wf_existeromana (integer ai_planta);Boolean 	lb_retorno
Integer	li_count

lb_Retorno	=	TRUE

SELECT IsNull(Count(*),0)
INTO :li_count
FROM  dbo.plantaconfromana
WHERE  plde_codigo =: ai_planta
AND crpl_equcon =: gstr_us.computador;	 
	 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla plantaconfromana")
	lb_Retorno	=	TRUE	
ELSEIF  li_count = 0 THEN
	MessageBox("Atención", "Computador no Tiene Asignado Romana")
	lb_Retorno	=	TRUE
ELSE 
	lb_Retorno	=	FALSE
END IF

RETURN lb_Retorno
end function

public function boolean wf_bultosexistencia (integer ai_cantidad);Boolean	lb_Retorno = True
Integer	li_Planta, li_Camara, li_PlantaLote, li_EspecieLote, li_NumeroLote, &
			li_secuen
Decimal{3} ld_Saldo

li_Planta			=	dw_1.Object.plde_codigo[il_Fila]
li_Camara		=	dw_1.Object.cama_codigo[il_Fila]
li_PlantaLote	=	dw_1.Object.lofc_pltcod[il_Fila]
li_EspecieLote	=	dw_1.Object.lofc_espcod[il_Fila]
li_NumeroLote	=	dw_1.Object.lofc_lotefc[il_Fila]
li_secuen			=	dw_1.Object.lfcd_secuen[il_Fila]

SELECT	IsNull(Sum(caex_canbul), 0)
	INTO	:ld_Saldo
	FROM	dbo.spro_camaraexistecom
	WHERE plde_codigo	=	:li_Planta
	AND   cama_codigo	=	:li_Camara
	AND   lofc_pltcod	=	:li_PlantaLote
	AND   lofc_espcod	=	:li_EspecieLote
	AND   lofc_lotefc	=	:li_NumeroLote
	AND   lfcd_secuen	=	:li_secuen;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla de Existencia Camara Comercial")
	
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "No existe Tarja en Cámara especificada.~r~r" + &
					"Ingrese o seleccione otros antecedentes.")
	
	lb_Retorno	=	False
ELSEIF IsNull(ld_saldo) OR ld_saldo < ai_cantidad THEN
	MessageBox("Atención", "No hay Existencia suficiente en la tarja ingresada~r" + &
					"Ingrese una cantidad mas baja.")
	lb_Retorno	=	False
ELSE
	ii_cantidad	=	ai_cantidad
END IF

RETURN lb_Retorno
end function

public subroutine wf_buscalote ();str_busqueda	lstr_busq
String			ls_Nula

SetNull(ls_Nula)

lstr_busq.argum[1]	=	String(dw_1.Object.lofc_pltcod[il_Fila])
lstr_busq.argum[2]	=	String(dw_1.Object.cama_codigo[il_Fila])
lstr_busq.argum[3]	=	String(dw_1.Object.lofc_espcod[il_Fila])

IF istr_mant.argumento[5] <> "" and istr_mant.argumento[2] = "32" THEN
   lstr_busq.argum[4]	=	istr_mant.argumento[5]
ELSE
	lstr_busq.argum[4]	=	""
END IF	

OpenWithParm(w_busc_lotesfrutacomdeta_venta, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[3] <> "" THEN
	
	IF Not iuo_Lote.Existe(integer(lstr_busq.argum[1]), &
								  integer(lstr_busq.argum[2]), long(lstr_busq.argum[3]), &
								  integer(lstr_busq.argum[4]),True, sqlca) THEN
			dw_1.SetItem(il_Fila, "lofc_lotefc", Long(ls_Nula))
			RETURN 
	ELSEIF Duplicado("lofc_lotefc", lstr_busq.argum[3]) THEN
			 dw_1.SetItem(il_Fila, "lofc_lotefc", Long(ls_Nula))
			
			RETURN
	ELSE
	   IF istr_mant.argumento[5]<>"" and istr_mant.argumento[2] = "32" THEN
			IF Integer(istr_mant.argumento[5])<>iuo_lote.Productor THEN
				Messagebox("Error de Consistencia","El Lote Seleccionado No Pertenece al Productor-Cliente.")
				dw_1.SetItem(il_Fila, "lofc_lotefc", Integer(ls_Nula))
				RETURN 
			END IF		
		END IF  
		dw_1.Object.lofc_pltcod[il_Fila] 	=	Integer(lstr_busq.argum[1])
		dw_1.Object.lofc_espcod[il_Fila] 	=	Integer(lstr_busq.argum[2])
		dw_1.Object.lofc_lotefc[il_Fila] 	=	Long(lstr_busq.argum[3])
		dw_1.Object.lfcd_secuen[il_Fila] 	=	integer(lstr_busq.argum[4])
		dw_1.Object.vari_codigo[il_Fila] 	=	integer(lstr_busq.argum[5])
		dw_1.Object.vari_nombre[il_Fila] 	=	lstr_busq.argum[6]
		dw_1.Object.prod_codigo[il_Fila] 	=	Long(lstr_busq.argum[7])
		dw_1.Object.prod_nombre[il_Fila]	=	lstr_busq.argum[8]
		dw_1.Object.cama_codigo[il_Fila] =	Integer(lstr_busq.argum[14])
		dw_1.Object.enva_tipoen[il_Fila] 	=	Integer(lstr_busq.argum[15])
		dw_1.Object.enva_codigo[il_Fila] 	=	Integer(lstr_busq.argum[16])
		dw_1.Object.enva_nombre[il_Fila]	=	lstr_busq.argum[17]
		dw_1.Object.mfcd_calibr[il_Fila] 	=	lstr_busq.argum[18]
		dw_1.Object.fgmb_nrotar[il_Fila] 	=	Long(lstr_busq.argum[19])
		dw_1.Object.cale_calida[il_Fila] 	=	lstr_busq.argum[22]
		
		ii_cantidad								=	Long(lstr_busq.argum[13])
		ii_cantori									=	Long(lstr_busq.argum[21])
		id_pesnet1								=	(iuo_Lote.KilosPromed)
		id_lfcd_kilobp							=	Round(dec(lstr_busq.argum[23]),2)
		is_calidad								=	lstr_busq.argum[22]
		
		dw_1.Object.nrotar2[il_Fila] 		=	Long(lstr_busq.argum[21])
		dw_1.Object.mfcd_bulent[il_Fila] 	=	Round(dec(lstr_busq.argum[13]),2)
		dw_1.Object.mfcd_bulent[il_Fila] 	=	Round(dec(lstr_busq.argum[13]),2)
		dw_1.Object.mfcd_kilrom[il_Fila] 	=	Round(dec(lstr_busq.argum[20]),3	)
		dw_1.Object.mfcd_kgnent[il_Fila]	=	Round(Dec(lstr_busq.argum[13]) * id_pesnet1,3)
		
		IF ii_cantori = 1 THEN
			ib_control 											= 	True
		
			dw_1.Object.mfcd_bulent.Protect				=	1
			dw_1.Object.mfcd_bulent.BackGround.Color	=	rgb(166,180,210)
		ELSE
			ib_control											=	False
			
			dw_1.Object.mfcd_bulent.Protect				=	0
			dw_1.Object.mfcd_bulent.BackGround.Color	=	RGB(255,255,255)
		END IF

		wf_precio_unitario(Date(istr_mant.argumento[13]),Integer(lstr_busq.argum[2]),&
		                integer(lstr_busq.argum[5]),lstr_busq.argum[18])
	END IF
END IF

RETURN
end subroutine

public subroutine wf_buscalote2 ();str_busqueda	lstr_busq
String			ls_Nula

SetNull(ls_Nula)

IF istr_Mant.Argumento[4]	=	'1' THEN
	lstr_Busq.Argum[1]	=	String(dw_1.Object.lofc_pltcod[il_Fila])
	lstr_Busq.Argum[2]	=	String(dw_1.Object.lofc_espcod[il_Fila])

	OpenWithParm(w_busc_lotesfrutacomdeta_recepcion, lstr_busq)
ELSE
	lstr_Busq.Argum[1]	=	String(dw_1.Object.lofc_pltcod[il_Fila])
	lstr_Busq.Argum[2]	=	String(dw_1.Object.cama_codigo[il_Fila])
	lstr_Busq.Argum[3]	=	String(dw_1.Object.lofc_espcod[il_Fila])
	IF istr_mant.argumento[2]="25" THEN
		lstr_Busq.Argum[4]   = "25"
	ELSE	
		lstr_Busq.Argum[4]   =  Istr_Mant.Argumento[6]
	END IF	

	OpenWithParm(w_busc_lotesfrutacomdeta_despacho, lstr_busq)
	
END IF

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[4] = "" THEN
	dw_1.SetColumn("lofc_lotefc")
	dw_1.SetFocus()
ELSE
	dw_1.Object.lofc_lotefc[il_Fila] =	Long(lstr_busq.argum[3])
	dw_1.Object.lfcd_secuen[il_Fila] =	Integer(lstr_Busq.Argum[4])
	dw_1.Object.vari_nombre[il_Fila] =	lstr_busq.argum[6]
	dw_1.Object.prod_nombre[il_Fila] =	lstr_busq.argum[8]

	iuo_Lote.Existe(dw_1.Object.lofc_pltcod[il_Fila], dw_1.Object.lofc_espcod[il_Fila], &
							long(lstr_Busq.Argum[3]),Integer(lstr_busq.argum[4]), &
							False, sqlca)

	IF istr_Mant.Argumento[4]	=	'2' THEN
		dw_1.Object.cama_codigo[il_Fila] =	Integer(lstr_Busq.Argum[14])
		dw_1.Object.mfcd_bulent[il_Fila] =	Dec(lstr_busq.argum[13])
		dw_1.Object.mfcd_kgnent[il_Fila]	=	Dec(lstr_busq.argum[13]) * iuo_Lote.KilosPromed
		dw_1.Object.cale_calida[il_Fila]	=	lstr_busq.argum[18]
	END IF

	IF Duplicado("lofc_lotefc", lstr_busq.argum[3]) THEN
		dw_1.SetItem(il_Fila, "lofc_lotefc", Long(ls_Nula))
		dw_1.SetItem(il_Fila, "lfcd_secuen", Integer(ls_Nula))
		dw_1.SetItem(il_Fila, "vari_nombre", ls_Nula)
		dw_1.SetItem(il_Fila, "prod_nombre", ls_Nula)
		dw_1.SetItem(il_Fila, "mfcd_bulent", 0)
		dw_1.SetItem(il_Fila, "mfcd_kgnent", 0)
	END IF	
END IF

RETURN
end subroutine

public subroutine wf_buscaenvase ();str_busqueda	lstr_busq
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

public function boolean wf_cargatarja (long al_tarja, integer al_cliente, integer ai_planta);Integer			li_planta, li_especie, li_lote, li_secuencia, li_variedad, li_productor, &
					li_tipoenvase, li_envase, li_bultos, li_camara, li_control, li_bultosex, li_estado
Long				ll_productor, ll_filas, secue1, secue2, secue3
Dec{9}			ldc_neto
String				ls_envanombre, ls_varinombre, ls_prodnombre, ls_Calibre

uo_variedades	luo_variedades
uo_productores	luo_productores
uo_envases		luo_envases

If dw_1.Find("fgmb_nrotar = " + String(al_tarja) + " or " + "tarja2 = " 		+ String(al_tarja) + " or " + &
				 "tarja3 = " 		+ String(al_tarja), 1, dw_1.RowCount()) > 0 Then
	MessageBox("Atención", "La Tarja ya ha sido ingresada .~r~r" + "Ingrese o seleccione otra.", StopSign!)
	Return False
End If

Select lofc_pltcod, lofc_espcod,	lofc_lotefc, lfcd_secuen, 	vari_codigo,
		 prod_codigo, enva_tipoen,	enva_codigo, lfcd_bultos,	lfcd_kilnet,
		 refe_gcalib, cale_calida
Into 	:li_planta,   :li_especie,   :li_lote,  :li_secuencia,:li_variedad, 
		:ll_productor,:li_tipoenvase,:li_envase,:li_bultos, 	:ldc_neto,
		:ls_Calibre,  :is_calidad
From dbo.spro_lotesfrutacomdeta
where fgmb_nrotar =	:al_tarja
   And clie_codigo = :al_Cliente
   And lofc_pltcod = :ai_Planta;

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca, "Lectura tabla de Detalle de Lotes Comerciales")
	Return False
ElseIf sqlca.SQLCode = 100 Then
	MessageBox("Atención", "No existe la Tarja ingresada, para Cliente Seleccionado..~r~r" + "Ingrese o seleccione otra.")
	Return False
End If

ii_cantori	=	li_bultos

Select Count(*), Min(fgmb_estado)
  into :li_control, :li_estado
  from dbo.spro_movtobins
 where fgmb_nrotar =:al_tarja
    And plde_codigo = :ai_planta;

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca, "Lectura tabla de Detalle de Movimientos de Bins")
	Return False
ElseIf li_control > 1 Then
	MessageBox("Atención", "La Tarja esta duplicada en la tabla de movimientos de Bins.~r~r" + &
					"Ingrese o seleccione otra.")
	Return False
ElseIf li_estado <> 2 Then
	MessageBox("Atención", "La Tarja no esta activa, ya fue despachada en su totalidad.~r~r" + &
					"Ingrese o seleccione otra.")
	Return False
End If

If li_bultos = 1 Then
	dw_1.Object.tarja2.Protect						=	0
	dw_1.Object.tarja2.Color						=	0
	dw_1.Object.tarja2.BackGround.Color			=	RGB(255,255,255)
	
	dw_1.Object.tarja3.Protect						=	0
	dw_1.Object.tarja3.Color						=	0
	dw_1.Object.tarja3.BackGround.Color			=	RGB(255,255,255)
	
	dw_1.Object.mfcd_bulent.Protect				=	1
	dw_1.Object.mfcd_bulent.Color				=	RGB(255,255,255)
	dw_1.Object.mfcd_bulent.BackGround.Color	=	553648127
	ib_control 											= 	True
Else
	If Integer(dw_1.Object.mfcd_bulent[il_fila]) > 0 Then
		MessageBox("Error", "Ya existen bultos en esta fila de venta, ingrese la tarja en otra fila", Stopsign!)
		Return False
	Else
		ib_control											=	False
		ldc_neto												=	ldc_neto / li_bultos
		dw_1.Object.mfcd_bulent[il_fila]				=	li_bultos
		dw_1.Object.tarja2.Protect						=	1
		dw_1.Object.tarja2.Color						=	RGB(255,255,255)
		dw_1.Object.tarja2.BackGround.Color			=	553648127
		
		dw_1.Object.tarja3.Protect						=	1
		dw_1.Object.tarja3.Color						=	RGB(255,255,255)
		dw_1.Object.tarja3.BackGround.Color			=	553648127
		
		dw_1.Object.mfcd_bulent.Protect				=	0
		dw_1.Object.mfcd_bulent.Color				=	0
		dw_1.Object.mfcd_bulent.BackGround.Color	=	RGB(255,255,255)
	End If
End If

luo_envases	=	Create uo_envases
luo_variedades 	= 	Create uo_variedades
luo_productores	=	Create uo_productores

If Not luo_envases.Existe(li_tipoenvase,li_envase, True, SQLCa) Then Return False
If Not luo_variedades.Existe(li_especie, li_variedad, true, sqlca) Then Return False
If Not luo_productores.Existe(ll_productor, true, sqlca) Then Return False


Select cama_codigo
Into :li_camara
from dbo.spro_movtobins
where plde_codigo =  :li_planta
  and fgmb_nrotar	=	:al_tarja
  and fgmb_estado	=	2;
	
If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca, "Lectura tabla de Movimiento de Bins")
	Return False
ElseIf sqlca.SQLCode = 100 Then
	MessageBox("Atención", "No existen Movimientos asociados a la tarja .~r~r" + &
					"Ingrese o seleccione otra.")
	Return False
End If

Select cama_codigo, caex_canbul
Into :li_camara, :li_bultosex
From dbo.spro_camaraexistecom
where plde_codigo	=  :li_planta
  and lofc_pltcod =	:li_planta
  and lofc_espcod	=	:li_especie
  and lofc_lotefc	=	:li_lote
  and lfcd_secuen	=	:li_secuencia;
	
If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca, "Lectura tabla de Movimiento de Existencia")
	Return False
ElseIf sqlca.SQLCode = 100 Then
	MessageBox("Atención", "No existe Tarja en existencias.~r~rIngrese o seleccione otra.")
	Return False
ElseIf li_bultosex < 1 Then
	MessageBox("Atención", "No hay datos en existencia para esta Tarja.~r~rIngrese o seleccione otra.")
	Return False
End If

If Long(dw_1.Object.lfcd_secue2[il_fila]) > 0 Then li_Bultos ++
If Long(dw_1.Object.lfcd_secue3[il_fila]) > 0 Then li_Bultos ++

If Long(dw_1.Object.lofc_lotefc[il_fila]) <> 0 AND ib_control Then
	If ( dw_1.Object.cama_codigo[il_fila] <>	li_camara)  Then
		MessageBox("Error", "La Tarja Ingresada No Corresponde a la Camara de los otros Bins del pesaje actual,"+&
									"~r~n Ingreselos por separado")
		Return False
	ElseIf ( dw_1.Object.lofc_pltcod[il_fila] <>	li_planta) Then
		MessageBox("Error", "La Tarja Ingresada No Corresponde a la Planta de los otros Bins del pesaje actual,"+&
									"~r~n Ingreselos por separado")
		Return False
	ElseIf ( dw_1.Object.lofc_espcod[il_fila] <>	li_especie) Then
		MessageBox("Error", "La Tarja Ingresada No Corresponde a la Especie de los otros Bins del pesaje actual,"+&
									"~r~n Ingreselos por separado")
		Return False
	ElseIf ( dw_1.Object.lofc_lotefc[il_fila] <>	li_lote) Then
		MessageBox("Error", "La Tarja Ingresada No Corresponde al Lote de los otros Bins del pesaje actual,"+&
									"~r~n Ingreselos por separado")
		Return False
	ElseIf ( dw_1.Object.enva_tipoen[il_fila] <> li_tipoenvase ) Then
		MessageBox("Error", "Los Tipos de Envases no son coherentes entre los bultos del pesaje,"+&
									"~r~n Ingreselos por separado")
		Return False
	ElseIf ( dw_1.Object.enva_codigo[il_fila] <> li_envase ) Then
		MessageBox("Error", "Los Codigos de envases no son coherentes entre los bultos del pesaje,"+&
									"~r~n Ingreselos por separado")
		Return False
	ElseIf ( dw_1.Object.vari_codigo[il_fila] <> li_variedad ) Then
		MessageBox("Error", "Las Variedades no son coherentes entre los bultos del pesaje,"+&
									"~r~n Ingreselos por separado")
		Return False
	ElseIf ( dw_1.Object.mfcd_calibr[il_fila] 	<>  ls_Calibre ) Then
		MessageBox("Error", "Los Grupos de Calibres no son coherentes entre los bultos del pesaje,"+&
									"~r~n Ingreselos por separado")
		Return False
	End If
End If

dw_1.Object.cama_codigo[il_fila]	=	li_camara
dw_1.Object.lofc_pltcod[il_fila]		=	li_planta
dw_1.Object.lofc_espcod[il_fila]	=	li_especie
dw_1.Object.lofc_lotefc[il_fila]		=	li_lote
dw_1.Object.vari_nombre[il_fila]	=	luo_variedades.NombreVariedad
dw_1.Object.prod_nombre[il_fila]	=	luo_productores.Nombre
dw_1.Object.prod_codigo[il_fila]	=	luo_productores.Codigo
dw_1.Object.enva_tipoen[il_fila]	=	li_tipoenvase
dw_1.Object.enva_codigo[il_fila]	=	li_envase
dw_1.Object.enva_nombre[il_fila]	=	luo_envases.Nombre
dw_1.Object.cale_calida[il_fila]		=	is_calidad
dw_1.Object.vari_codigo[il_fila] 	=  li_variedad
dw_1.Object.mfcd_calibr[il_fila] 	=  ls_Calibre

If ib_control Then 
	dw_1.Object.mfcd_bulent[il_fila]	=	li_bultos
Else
	dw_1.Object.mfcd_bulent[il_fila]	=	li_bultosex
	ii_cantidad								=	li_bultosex
End If

If is_columna	=	"fgmb_nrotar" Then
	dw_1.Object.lfcd_secuen[il_fila]	=	li_secuencia
	id_pesnet1								=	ldc_neto
ElseIf is_columna	=	"tarja2" Then
	dw_1.Object.lfcd_secue2[il_fila]	=	li_secuencia
	id_pesnet2 								=	ldc_neto
ElseIf is_columna	=	"tarja3" Then
	dw_1.Object.lfcd_secue3[il_fila]	=	li_secuencia
	id_pesnet3								=	ldc_neto
End If

wf_Precio_Unitario(Date(istr_mant.argumento[13]),li_especie,li_variedad,ls_calibre)

Destroy luo_envases;
Destroy luo_variedades;
Destroy luo_productores;

dw_1.AcceptText()

iuo_Lote.Existe(dw_1.Object.lofc_pltcod[il_fila], dw_1.Object.lofc_espcod[il_fila],&
					dw_1.Object.lofc_lotefc[il_fila], dw_1.Object.lfcd_secuen[il_fila],True, sqlca)
					
If NOT wf_HayExistencia("", "", li_bultos) Then
	SetNull(li_bultos)
	
	dw_1.Object.cama_codigo[il_fila]		=	li_bultos
	dw_1.Object.lofc_pltcod[il_fila]			=	li_bultos
	dw_1.Object.lofc_espcod[il_fila]		=	li_bultos
	dw_1.Object.lofc_lotefc[il_fila]			=	li_bultos
	
	If is_columna	=	"fgmb_nrotar" Then
		dw_1.Object.lfcd_secuen[il_fila]	=	li_bultos
	ElseIf is_columna	=	"tarja2" Then
		dw_1.Object.lfcd_secue2[il_fila]	=	li_bultos
	ElseIf is_columna	=	"tarja3" Then
		dw_1.Object.lfcd_secue3[il_fila]	=	li_bultos
	End If

	dw_1.Object.vari_nombre[il_fila]		=	""
	dw_1.Object.prod_nombre[il_fila]		=	""
	dw_1.Object.enva_tipoen[il_fila]		=	li_bultos
	dw_1.Object.enva_codigo[il_fila]		=	li_bultos
	dw_1.Object.enva_nombre[il_fila]		=	""
	dw_1.Object.mfcd_bulent[il_fila]		=	li_bultos
	dw_1.Object.mfcd_kgnent[il_fila]		=	Dec(li_bultos)
	dw_1.Object.vari_codigo[il_fila] 		=  li_bultos
	dw_1.Object.mfcd_calibr[il_fila] 		=	""
	dw_1.SetColumn(is_columna)
	dw_1.SetFocus()
	Return False
Else
	Return True
End If

end function

on w_mant_deta_movtofrutcomer_despachoventa.create
int iCurrent
call super::create
this.ole_puerta=create ole_puerta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.ole_puerta
end on

on w_mant_deta_movtofrutcomer_despachoventa.destroy
call super::destroy
destroy(this.ole_puerta)
end on

event ue_recuperadatos;call super::ue_recuperadatos;If istr_mant.Argumento[2]='21' Then
   	dw_1.Object.lofc_espcod[il_Fila]					=	integer(istr_mant.Argumento[8])
	dw_1.Object.lofc_espcod.Protect 					=	1
	dw_1.Object.lofc_espcod.Color 					=	Rgb(255,255,255)
	dw_1.Object.lofc_espcod.BackGround.Color 	=	553648127
	dw_1.Object.prod_nombre[il_fila]					=	istr_mant.argumento[7]
	dw_1.Object.prod_nombre.Protect 				=	1
	dw_1.Object.prod_nombre.Color 					=	Rgb(255,255,255)
	dw_1.Object.prod_nombre.BackGround.Color 	=	553648127
	dw_1.Object.vari_nombre[il_fila]					=	istr_mant.argumento[10]
	dw_1.Object.vari_nombre.Protect 					=	1
	dw_1.Object.vari_nombre.Color 					=	Rgb(255,255,255)
	dw_1.Object.vari_nombre.BackGround.Color 	=	553648127
End If	

dw_1.Object.Valores[il_fila]	=	Integer(istr_Mant.Argumento[12])

ias_campo[1]	=	String(dw_1.object.plde_codigo[il_fila])
ias_campo[2]	=	String(dw_1.object.cama_codigo[il_fila])
ias_campo[3]	=	String(dw_1.object.lofc_pltcod[il_fila])
ias_campo[4]	=	String(dw_1.object.lofc_espcod[il_fila])
ias_campo[5]	=	String(dw_1.object.lofc_lotefc[il_fila])
ias_campo[6]	=	String(dw_1.object.mfcd_bulent[il_fila])
ias_campo[7]	=	String(dw_1.object.mfcd_kgnent[il_fila])
ias_campo[8]	=	String(dw_1.object.clie_codigo[il_fila])

If istr_mant.Agrega Then
	dw_1.Object.clie_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[16])
	dw_1.Object.plde_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[1])
	dw_1.Object.plde_coorde[il_Fila]	=	Integer(istr_Mant.Argumento[1])
	dw_1.Object.tpmv_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[2])
	dw_1.Object.mfco_numero[il_Fila]=	Integer(istr_Mant.Argumento[3])
	dw_1.object.lofc_pltcod[il_fila]		=	gstr_ParamPlanta.CodigoPlanta
	dw_1.object.lofc_espcod[il_fila]	=	gstr_ParamPlanta.CodigoEspecie
//	dw_1.Object.lfcd_secuen[il_Fila]	=	1
	
	iuo_PltaLote.Existe(gstr_ParamPlanta.CodigoPlanta, True, sqlca)
	iuo_Especie.Existe(gstr_ParamPlanta.CodigoEspecie, True, sqlca)
ElseIf Not istr_mant.Agrega AND Not istr_mant.Borra Then
	iuo_PltaLote.Existe(dw_1.object.lofc_pltcod[il_fila], True, sqlca)
	iuo_Especie.Existe(dw_1.object.lofc_espcod[il_fila], True, sqlca)
	iuo_Lote.Existe(iuo_PltaLote.Codigo, iuo_Especie.Codigo, dw_1.object.lofc_lotefc[il_fila], &
						dw_1.object.lfcd_secuen[il_fila],True, sqlca)
	ExisteVariedad(iuo_Especie.Codigo, iuo_Lote.Variedad, istr_Variedad)
	iuo_Productor.Existe(iuo_Lote.Productor, True, sqlca)
End If
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	//dw_1.object.plde_coorde[il_fila]	=	Integer(ias_campo[1])
	dw_1.object.cama_codigo[il_fila]	=	Integer(ias_campo[2])
	dw_1.object.lofc_pltcod[il_fila]		=	Integer(ias_campo[3])
	dw_1.object.lofc_espcod[il_fila]	=	Integer(ias_campo[4])
	dw_1.object.lofc_lotefc[il_fila]		=	Integer(ias_campo[5])
	dw_1.object.mfcd_bulent[il_fila]	=	dec(ias_campo[6])
	dw_1.object.mfcd_kgnent[il_fila]	=	Dec(ias_campo[7])
	dw_1.object.clie_codigo[il_fila]		=	Integer(ias_campo[8])
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

event ue_nuevo;call super::ue_nuevo;//Agrega L.M.
Integer	li_null

SetNull(li_null)

IF istr_mant.Argumento[2]='21' THEN
   	dw_1.Object.lofc_espcod[il_Fila]	=	integer(istr_mant.Argumento[8])
	dw_1.Object.prod_nombre[il_fila]	=	istr_mant.argumento[7]
	dw_1.Object.vari_nombre[il_fila]	=	istr_mant.argumento[10]
ELSE
   	dw_1.Object.lofc_espcod[il_Fila]	=	gstr_ParamPlanta.CodigoEspecie
	iuo_Especie.Existe(gstr_ParamPlanta.CodigoEspecie, True, sqlca)
END IF

dw_1.Object.plde_codigo[il_Fila]		=	Integer(istr_Mant.Argumento[1])
dw_1.Object.plde_coorde[il_Fila]		=	Integer(istr_Mant.Argumento[1])
dw_1.Object.tpmv_codigo[il_Fila]		=	Integer(istr_Mant.Argumento[2])
dw_1.Object.mfco_numero[il_Fila]	=	Integer(istr_Mant.Argumento[3])
dw_1.Object.clie_codigo[il_Fila]		=	Integer(istr_Mant.Argumento[16])
dw_1.Object.lofc_pltcod[il_Fila]		=	gstr_ParamPlanta.CodigoPlanta
dw_1.Object.lfcd_secuen[il_Fila]		=	1

dw_1.GetChild("lofc_pltcod", idwc_Planta)
idwc_Planta.SetTransObject(sqlca)
idwc_Planta.Retrieve()
idwc_Planta.SetSort("plde_nombre A")
idwc_Planta.Sort()

iuo_PltaLote.Existe(gstr_ParamPlanta.CodigoPlanta, True, sqlca)

end event

event open;/* 
	Argumentos
	istr_Mant.Argumento[01]	=	Código Planta
	istr_Mant.Argumento[02]	=	Tipo de Movimiento
	istr_Mant.Argumento[03]	=	Número de Despacho
	istr_Mant.Argumento[04]	=	Sentido del Movimiento => 2 = Despacho
	istr_Mant.Argumento[09]	=	Tipo Documento
	istr_Mant.Argumento[10]	=	Documento Relacionado (Orden de Venta)
	istr_Mant.Argumento[11]	=	ventana de procedencia
	istr_mant.Argumento[12]	=	Tipo de Ingreso(0 = Neto - 1 = Bruto
	istr_Mant.Argumento[13]	=  Fecha de Movimiento
	istr_Mant.Argumento[14]	=  Fila
	istr_Mant.Argumento[20]	=	Tipo Ingreso
*/
Integer 	li_resultado, li_null
String	ls_parametros
Boolean	ib_OCX	=	True

x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

IF istr_Mant.Argumento[20] = "1" THEN
	dw_1.DataObject = "dw_mant_movtofrutacomdeta_despachoventa"
ELSE
	dw_1.DataObject = "dw_mant_movtofrutacomdeta_mermas"
END IF


iuo_PltaLote		=	Create uo_plantadesp
iuo_Especie		=	Create uo_especie
iuo_Lote			=	Create uo_lotesfrutacomer
iuo_Productor	=	Create uo_productores
iuo_Camara		=	Create uo_camarasbode
iuo_TipoMovto	=	Create uo_tipomovtofruta


dw_1.GetChild("cama_codigo", idwc_Camara)
idwc_Camara.SetTransObject(sqlca)
idwc_Camara.Retrieve(Integer(istr_Mant.Argumento[1]))
idwc_Camara.SetSort("cama_nombre A")
idwc_Camara.Sort()

dw_1.GetChild("lofc_pltcod", idwc_Planta)
idwc_Planta.SetTransObject(sqlca)
idwc_Planta.Retrieve()
idwc_Planta.SetSort("plde_nombre A")
idwc_Planta.Sort()

dw_1.GetChild("lofc_espcod", idwc_Especie)
idwc_Especie.SetTransObject(sqlca)
idwc_Especie.Retrieve()
idwc_Especie.SetSort("espe_nombre A")
idwc_Especie.Sort()

dw_1.GetChild("enva_tipoen", idwc_TipoEnvase)
idwc_TipoEnvase.SetTransObject(sqlca)
idwc_TipoEnvase.Retrieve()
idwc_TipoEnvase.SetSort("tien_nombre A")
idwc_TipoEnvase.Sort()
idwc_TipoEnvase.SetFilter("tien_usoenv = 1")
idwc_TipoEnvase.Filter()

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)

iuo_TipoMovto.Existe(Integer(istr_mant.Argumento[2]),True,SQLCA)

IF iuo_TipoMovto.Sentido = 2 THEN
	dw_1.Modify("titulo.Text = 'DETALLE DE DESPACHO'")
ELSE
	dw_1.Modify("titulo.Text = 'DETALLE DE RECEPCION'")
END IF

IF istr_Mant.Argumento[2] = '23' THEN
	dw_1.Object.mfcd_preuni.protect 					= 	1
	dw_1.Object.mfcd_preuni.Color					=	Rgb(255,255,255)	
	dw_1.Object.mfcd_preuni.BackGround.Color	=	553648127	
	dw_1.Object.mfcd_prebru.protect 					= 	1
	dw_1.Object.mfcd_prebru.Color					=	Rgb(255,255,255)	
	dw_1.Object.mfcd_prebru.BackGround.Color	=	553648127
ELSE
	IF istr_Mant.Argumento[12] = '1' THEN
		dw_1.Object.mfcd_preuni.protect 					= 	1
		dw_1.Object.mfcd_preuni.Color					=	Rgb(255,255,255)	
		dw_1.Object.mfcd_preuni.BackGround.Color	=	553648127
	ELSE
		dw_1.Object.mfcd_prebru.protect 					= 	1
		dw_1.Object.mfcd_prebru.Color					=	Rgb(255,255,255)	
		dw_1.Object.mfcd_prebru.BackGround.Color	=	553648127
	END IF
END IF

If istr_Mant.Argumento[2] = '34' Then
	IF istr_Mant.Argumento[20] = "1" THEN
		dw_1.Object.t_6.Visible				=	False
		dw_1.Object.t_12.Visible			=	False
		dw_1.Object.mfcd_prebru.visible	=	False
		dw_1.Object.mfcd_preuni.visible	=	False
	Else
		dw_1.Object.t_13.Visible			=	False
		dw_1.Object.t_14.Visible			=	False
		dw_1.Object.mfcd_prebru.visible	=	False
		dw_1.Object.mfcd_preuni.visible	=	False		
	End If
End If

//li_resultado 	=	ConfiguracionPuerta(istr_puertacomm)
//
//IF li_resultado = 0 THEN
//	ls_parametros	=	String(istr_puertacomm.Baudios)+","+istr_puertacomm.Paridad+","+&
//							String(istr_puertacomm.Data)+","+String(istr_puertacomm.Parada)
//			
//	IF NOT ib_OCX THEN
//		MessageBox("Conexión Romana","No está instalado el OCX para conexión con Romana")
//	ELSE
//		IF Ole_puerta.object.PortOpen THEN Ole_puerta.object.PortOpen = False
//		Ole_puerta.object.settings	=	ls_parametros
//		Ole_puerta.object.PortOpen	= True	
//		Timer(0.2)
//	END IF
//END IF
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
//	dw_1.Object.romana[dw_1.GetRow()]	=	ld_kilos
//END IF
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_movtofrutcomer_despachoventa
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_movtofrutcomer_despachoventa
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_movtofrutcomer_despachoventa
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_movtofrutcomer_despachoventa
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_movtofrutcomer_despachoventa
integer x = 2112
integer y = 340
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_movtofrutcomer_despachoventa
integer x = 2112
integer y = 164
end type

event pb_acepta::clicked;If dw_1.Object.plde_codigo[1] = 41 Then
	If il_Fila > 20 Then
		MessageBox('Atencion', 'No se pueden ingresar mas de 20 registros.', Exclamation!, OK!)
		istr_mant.respuesta = 0
		CloseWithReturn(Parent, istr_mant)
	ElseIf dw_1.Object.mfcd_preuni[il_fila] > 0 AND istr_Mant.Argumento[2] <> '23' Then
		istr_mant.respuesta = 1
		
		If istr_mant.Agrega Then
			Parent.TriggerEvent("ue_nuevo")
		Else
			CloseWithReturn(Parent, istr_mant)
		End If
	ElseIf istr_Mant.Argumento[11]	=	'venta' Then
		MessageBox("Protección", "No ha ingresado el precio de venta del o los Bins")
		dw_1.SetColumn("mfcd_preuni")
		dw_1.SetFocus()
	Else
		istr_mant.respuesta = 1
		
		If istr_mant.agrega Then
			Parent.TriggerEvent("ue_nuevo")
		Else
			CloseWithReturn(Parent, istr_mant)
		End If	
	End If
ElseIf dw_1.Object.mfcd_preuni[il_fila] > 0 AND istr_Mant.Argumento[2] <> '23' Then
	istr_mant.respuesta = 1
	
	If istr_mant.Agrega Then
		Parent.TriggerEvent("ue_nuevo")
	Else
		CloseWithReturn(Parent, istr_mant)
	End If
ElseIf istr_Mant.Argumento[11]	=	'venta' Then
	MessageBox("Protección", "No ha ingresado el precio de venta del o los Bins")
	dw_1.SetColumn("mfcd_preuni")
	dw_1.SetFocus()
Else
	istr_mant.respuesta = 1
	
	If istr_mant.agrega Then
		Parent.TriggerEvent("ue_nuevo")
	Else
		CloseWithReturn(Parent, istr_mant)
	End If	
End If
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_movtofrutcomer_despachoventa
integer x = 2112
integer y = 504
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_movtofrutcomer_despachoventa
string tag = "Despacho Mermas"
integer x = 160
integer y = 172
integer width = 1810
integer height = 1312
string dataobject = "dw_mant_movtofrutacomdeta_despachoventa"
end type

event dw_1::itemchanged;String  	ls_Columna, ls_Nula
Long		ll_tarja

SetNull(ls_Nula)

ls_Columna = dwo.Name

Choose Case ls_Columna
	Case "cama_codigo"
		If Not iuo_Camara.Existe(Integer(istr_Mant.Argumento[1]), Integer(Data), True, sqlca) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		ElseIf Duplicado(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		Else
			If iuo_TipoMovto.Sentido = 2 Then
				If Not wf_HayExistencia(ls_Columna, Data, &
				This.Object.mfcd_bulent[Row]) Then
					This.SetItem(Row, ls_Columna, Integer(ls_Nula))
					Return 1
				End If
			End If
		End If

	Case "lofc_pltcod"
		If Not iuo_PltaLote.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		ElseIf Duplicado(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		Else
			If iuo_TipoMovto.Sentido = 2 Then
				If Not wf_HayExistencia(ls_Columna, Data, &
				This.Object.mfcd_bulent[Row]) Then
					This.SetItem(Row, ls_Columna, Integer(ls_Nula))
					Return 1
				End If
			End If
		End If

	Case "lofc_espcod"
		If Not iuo_Especie.Existe(Integer(Data), True, sqlca) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		ElseIf Duplicado(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		Else
			If Not wf_ExisteEnOrden(integer(istr_mant.Argumento[9]), Long(istr_mant.Argumento[10]),Integer(data)) Then
				This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			  Return 1
		   Else 
				If iuo_TipoMovto.Sentido = 2 Then
					If Not wf_HayExistencia(ls_Columna, Data, &
					This.Object.mfcd_bulent[Row]) Then
						This.SetItem(Row, ls_Columna, Integer(ls_Nula))
						Return 1
					End If
				End If
			End If	
		End If

	Case "lofc_lotefc"
		If Not iuo_Lote.Existe(iuo_PltaLote.Codigo, iuo_Especie.Codigo, Integer(Data), &
										/*dw_1.Object.lfcd_secuen[Row]*/-1,True, sqlca) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		ElseIf Duplicado(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		Else
			
			If istr_mant.argumento[5]<>"" Then
				If Integer(istr_mant.argumento[5])<>iuo_lote.Productor Then
					Messagebox("Error de Consistencia","El Lote Seleccionado No Pertenece al Productor-Cliente.")
					This.SetItem(Row, ls_Columna, Integer(ls_Nula))
					Return 1
				End If		
			End If	
			
			If Not wf_ExisteEnOrden(integer(istr_mant.Argumento[9]), Long(istr_mant.Argumento[10]),dw_1.Object.lofc_espcod[1]) Then
				This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			  Return 1
			End If
			
			If iuo_TipoMovto.Sentido = 2 Then
				This.Object.mfcd_bulent[Row] = 0
				If Not wf_HayExistencia(ls_Columna, Data, 0) Then
					This.SetItem(Row, ls_Columna, Integer(ls_Nula))
					Return 1
				End If
			End If

			ExisteVariedad(iuo_Especie.Codigo, iuo_Lote.Variedad, istr_Variedad)
			iuo_Productor.Existe(iuo_Lote.Productor, True, sqlca)
			
			This.Object.prod_nombre[Row] =	iuo_Productor.Nombre
			This.Object.prod_codigo[Row]	=	iuo_Lote.Productor
			This.Object.vari_nombre[Row]	=	istr_Variedad.Nombre
			This.Object.lfcd_secuen[Row]	=	Integer(ls_Nula)
		End If
	
	Case "lfcd_secuen"
		If Not iuo_Lote.Existe(iuo_PltaLote.Codigo,  iuo_Especie.Codigo, dw_1.Object.lofc_lotefc[Row], &
										Integer(data),True, sqlca) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		ElseIf Duplicado(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			Return 1
		Else
			If istr_mant.argumento[5]<>"" Then
				If Integer(istr_mant.argumento[5])<>iuo_lote.Productor Then
					Messagebox("Error de Consistencia","El Lote Seleccionado No Pertenece al Productor-Cliente.")
					This.SetItem(Row, ls_Columna, Integer(ls_Nula))
					Return 1
				End If		
			End If
			If iuo_TipoMovto.Sentido = 2 Then
				This.Object.mfcd_bulent[Row] = 0
				If Not wf_HayExistencia(ls_Columna, Data, 0) Then
					This.SetItem(Row, ls_Columna, Integer(ls_Nula))
					Return 1
				End If
			End If

			If Not wf_ExisteEnOrden(integer(istr_mant.Argumento[9]), Long(istr_mant.Argumento[10]),&
			                     dw_1.Object.lofc_espcod[1]) Then
				This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			  Return 1
			End If

			ExisteVariedad(iuo_Especie.Codigo, iuo_Lote.Variedad, istr_Variedad)
			iuo_Productor.Existe(iuo_Lote.Productor, True, sqlca)
			
			This.SetItem(Row, "prod_nombre", iuo_Productor.Nombre)
			This.SetItem(Row, "prod_codigo", iuo_Lote.Productor)
			This.SetItem(Row, "vari_nombre", istr_Variedad.Nombre)
			
			If Not ExisteEnvase(iuo_lote.tipoenvase, 0, istr_Envase) Then
				This.SetItem(Row, "enva_tipoen", Integer(ls_Nula))
			Else
			   This.SetItem(Row, "enva_tipoen", iuo_lote.tipoenvase)	
			End If	
			
			If Not ExisteEnvase(istr_Envase.TipoEnvase, iuo_lote.envase, istr_Envase) Then
				This.SetItem(Row, "enva_codigo", Integer(ls_Nula))
			Else	
				This.SetItem(Row, "enva_codigo", iuo_lote.envase)
				dw_1.Object.enva_nombre[Row]	=	istr_Envase.Nombre
		   End If	
			If NOT wf_CargaDatoSecuencia(Integer(data)) Then
				This.SetItem(Row, ls_Columna, Integer(ls_Nula))
				Return 1
			End If
		End If
		
		This.Object.mfcd_kilrom[Row]	=	This.Object.mfcd_bulent[Row] * iuo_Lote.Kilospromed
		This.Object.mfcd_kgnent[Row]	=	This.Object.mfcd_bulent[Row] * iuo_Lote.Kilospromed
		
	Case "enva_tipoen"
		If Not ExisteEnvase(Integer(Data), 0, istr_Envase) OR &
			Duplicado(ls_Columna, Data) OR istr_Envase.UsoEnvase <> 1 Then
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))

			Return 1
		End If

	Case "enva_codigo"
		If Not ExisteEnvase(istr_Envase.TipoEnvase, Integer(Data), istr_Envase) OR &
			Duplicado(ls_Columna, Data) Then
			This.SetItem(Row, ls_Columna, Integer(ls_Nula))
			
			Return 1
		Else
			dw_1.Object.enva_nombre[Row]	=	istr_Envase.Nombre
		End If
		
	Case "fgmb_nrotar", "tarja2", "tarja3"
		ib_flag		=	FALSE
		is_columna	=	dwo.name
		If ls_Columna = "tarja2" Then
			ll_tarja	=	dw_1.Object.fgmb_nrotar[Row]
			If IsNull(ll_tarja) OR ll_tarja = 0 Then
				MessageBox("Error", "Debe ingresar la primera tarja antes de ingresar la segunda", Exclamation!)
				This.SetItem(Row, ls_Columna, Long(ls_Nula))
				This.SetColumn("fgmb_nrotar")
				Return 1
			End If		
		ElseIf ls_Columna = "tarja3"  Then
			ll_tarja	=	dw_1.Object.tarja2[Row]
			If IsNull(ll_tarja) OR ll_tarja = 0 Then
				MessageBox("Error", "Debe ingresar la segunda tarja antes de ingresar la tercera", Exclamation!)
				This.SetItem(Row, ls_Columna, Long(ls_Nula))
				This.SetColumn("tarja2")
				Return 1
			End If
		End If
		
		If IsNull(data) OR data = "" Then 
			Return
		ElseIf NOT wf_CargaTarja(Long(data), This.Object.clie_codigo[Row], This.Object.plde_codigo[Row]) Then
			This.SetItem(Row, ls_Columna, Long(ls_Nula))
			Return 1
		End If
		
		wf_Calcula_Neto(2)
		
	Case "mfcd_prebru", "mfcd_preuni"
		wf_CalculaValores()
		
	Case "mfcd_bulent"
		If iuo_TipoMovto.Sentido = 2 Then
			If Integer(Data) > 0 Then
				If Not wf_BultosExistencia(Integer(Data)) Then
					This.Object.mfcd_bulent[Row]	=	ii_cantidad
		
					Return 1
				End If
				If istr_mant.Argumento[2] = '34' Then
					This.Object.mfcd_kilrom[Row]	=	Double(data) * iuo_Lote.Kilospromed
					This.Object.mfcd_kgnent[Row]	=	Double(data) * iuo_Lote.Kilospromed
					This.Object.romana[Row]		=	Double(data) * iuo_Lote.Kilospromed
				Else
					wf_Calcula_Neto(2)
				End If
			Else
				MessageBox("Error", "Imposible vender 0 Bultos.~r~nIngrese otra cIfra", StopSign!)
				This.Object.mfcd_bulent[Row]	=	ii_cantidad
	
				Return 1
			End If
		End If
		
	Case "romana"
		This.SetItem(Row, "mfcd_kilrom", Double(Data))
		wf_Calcula_Neto(1)
		
End Choose
end event

event dw_1::buttonclicked;call super::buttonclicked;String	ls_Boton

ls_Boton = dwo.Name

Choose Case ls_Boton
	Case "b_buscalotes"
		wf_BuscaLote()

	Case "b_buscaenvase"
		wf_BuscaEnvase()
		
	Case "b_romana"
		dw_1.AcceptText()
		dw_1.SetItem(Row, "mfcd_kilrom", dw_1.Object.romana[Row])
		wf_Calcula_Neto(1)
		
End Choose
end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;IF is_columna_anterior = "mfcd_preuni" OR is_columna_anterior = "mfcd_prebru" THEN
	wf_CalculaValores()
END IF

is_columna_anterior	=	dwo.name
end event

type ole_puerta from olecustomcontrol within w_mant_deta_movtofrutcomer_despachoventa
event oncomm ( )
boolean visible = false
integer x = 2085
integer y = 948
integer width = 174
integer height = 152
integer taborder = 60
boolean bringtotop = true
borderstyle borderstyle = stylelowered!
long backcolor = 16711680
boolean focusrectangle = false
string binarykey = "w_mant_deta_movtofrutcomer_despachoventa.win"
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
09w_mant_deta_movtofrutcomer_despachoventa.bin 
2B00000600e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe00000006000000000000000000000001000000010000000000001000fffffffe00000000fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
19w_mant_deta_movtofrutcomer_despachoventa.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
