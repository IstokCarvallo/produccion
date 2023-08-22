$PBExportHeader$w_mant_deta_lotesfrutacomdeta.srw
forward
global type w_mant_deta_lotesfrutacomdeta from w_mant_detalle_csd
end type
type ole_puerta from olecustomcontrol within w_mant_deta_lotesfrutacomdeta
end type
type dw_9 from datawindow within w_mant_deta_lotesfrutacomdeta
end type
type str_pesaje from structure within w_mant_deta_lotesfrutacomdeta
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

global type w_mant_deta_lotesfrutacomdeta from w_mant_detalle_csd
integer width = 2734
integer height = 2012
boolean righttoleft = true
ole_puerta ole_puerta
dw_9 dw_9
end type
global w_mant_deta_lotesfrutacomdeta w_mant_deta_lotesfrutacomdeta

type variables
uo_plantadesp			iuo_PltaLote
uo_especie				iuo_Especie
uo_lotesfrutacomer	iuo_LoteFC
uo_productores			iuo_Productor
uo_camarasfrigo		iuo_Camara
uo_tipomovtofruta		iuo_TipoMovto
uo_tratamientofrio	iuo_tipofrio
uo_periodofrio			iuo_periodofrio
uo_predios				iuo_predio
uo_prodcuarteles		iuo_cuartel
uo_categorias			iuo_categoria
uo_calicosechero		iuo_calidad
uo_bins					iuo_bins

str_variedad			istr_Variedad
str_envase				istr_Envase
str_puertacomm			istr_puertacomm

DataWindowChild		idwc_Planta, idwc_Especie, idwc_tipofrio, idwc_periodofrio, &
							idwc_tipoenvase, idwc_predio, idwc_cuartel, idwc_categoria, &
							idwc_calidad, idwc_bins
Private:
str_pesaje				wstr_pesaje
end variables

forward prototypes
public function boolean duplicado (string as_columna, string as_valor)
public function boolean existelote (string as_columna, string as_valor)
public subroutine calcula (string as_columna, integer ai_tipo)
public subroutine buscaproductor ()
public subroutine buscavariedad ()
public function integer retiraprod (string as_columna, string as_valor)
public subroutine buscaenvase ()
public function decimal obtienekilosant (long al_tarja)
public function boolean existetarja (integer ai_cliente, integer ai_planta, long al_tarja)
end prototypes

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

public subroutine calcula (string as_columna, integer ai_tipo);Decimal{2}	ld_Bultos, ld_KilNeto, ld_KilProm

dw_1.AcceptText()

ld_Bultos	= dw_1.Object.lfcd_bultos[il_Fila]
ld_KilNeto	= dw_1.Object.lfcd_kilnet[il_Fila]

CHOOSE CASE ai_Tipo
	CASE 1
		ld_Bultos	=	Dec(as_Columna)
		
	CASE 2
		ld_KilNeto	=	Dec(as_Columna)
		
END CHOOSE

IF IsNull(ld_Bultos) OR IsNull(ld_KilNeto) THEN RETURN

ld_KilProm	=	Round(ld_KilNeto / ld_Bultos, 2)

dw_1.SetItem(il_Fila, "lfcd_kilpro", ld_KilProm)

w_maed_movtofrutacomer_reproceso.Totales()
end subroutine

public subroutine buscaproductor ();Str_Busqueda	lstr_Busq
String ls_Nula

SetNull(ls_Nula)

OpenWithParm(w_busc_productores, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	
	//llama la funcion retiraprod, entregando los argumentos
	//y evaluando si retira el Prod. para asignar valor a "lofc_Tipool"
	IF retiraprod("prod_codigo",lstr_Busq.Argum[1]) = 2 THEN
							  
		IF istr_mant.argumento[12] = "" THEN			  
  			dw_1.Object.lfcd_tipool[il_fila] =  2
			istr_mant.argumento[12] = "2"
		ELSEIF istr_mant.argumento[12] = "2" THEN
				dw_1.Object.lfcd_tipool[il_fila] =  2
		ELSE
			MessageBox("Error de Tipo de Pool","Ingreso de Pool Distinto al Inicial (Retiro).~r Esta Información debe ser Ingresada en Otro Lote.")
			dw_1.SetItem(il_fila, "prod_codigo", long(ls_Nula))
			RETURN 
		END IF	
	ELSE
		IF istr_mant.argumento[12] = "" THEN			  
			dw_1.Object.lfcd_tipool[il_fila] =  1
			istr_mant.argumento[12] = "1"
		ELSEIF istr_mant.argumento[12] = "1" THEN
			dw_1.Object.lfcd_tipool[il_fila] =  1
		ELSE
			MessageBox("Error de Tipo de Pool","Ingreso de Pool Distinto al Inicial (Venta).~r Esta Información debe ser Ingresada en Otro Lote.")
			dw_1.SetItem(il_fila, "prod_codigo", long(ls_Nula))
			RETURN 
		END IF	
	END IF    
	
	dw_1.SetItem(il_Fila, "prbr_codpre", Integer(ls_Nula))
	dw_1.SetItem(il_Fila, "prcc_codigo", Integer(ls_Nula))
	
	dw_1.SetItem(il_Fila, "prod_codigo", long(lstr_Busq.Argum[1]))
	dw_1.SetItem(il_Fila, "prod_nombre", lstr_Busq.Argum[2])
   
	dw_1.GetChild("prbr_codpre",idwc_predio)
	idwc_predio.SetTransObject(SQLCA)
	IF idwc_predio.Retrieve(long(lstr_Busq.Argum[1])) = 0 THEN
		idwc_predio.InsertRow(0)
	END IF
	
	dw_1.SetColumn("prbr_codpre")
	dw_1.SetFocus()
ELSE
	dw_1.SetColumn("prod_codigo")
	dw_1.SetFocus()
END IF
end subroutine

public subroutine buscavariedad ();Str_Busqueda	lstr_Busq

String ls_Nula

SetNull(ls_Nula)

lstr_Busq.argum[1] = istr_mant.argumento[7]
lstr_Busq.argum[2] = "D"

OpenWithParm(w_busc_variedades, lstr_Busq)

lstr_Busq	= Message.PowerObjectParm

IF lstr_Busq.Argum[1] <> "" THEN
	
	//llama la funcion retiraprod, entregando los argumentos
	//y evaluando si retira el Prod. para asignar valor a "lofc_Tipool"
	IF retiraprod("vari_codigo",lstr_Busq.Argum[3]) = 2 THEN
							  
		IF istr_mant.argumento[12] = "" THEN			  
  			dw_1.Object.lfcd_tipool[il_fila] =  2
			istr_mant.argumento[12] = "2"
		ELSEIF istr_mant.argumento[12] = "2" THEN
				dw_1.Object.lfcd_tipool[il_fila] =  2
		ELSE
			MessageBox("Error de Tipo de Pool","Ingreso de Pool Distinto al Inicial (Retiro).~r Esta Información debe ser Ingresada en Otro Lote.")
			dw_1.SetItem(il_fila, "vari_codigo", Integer(ls_Nula))
			RETURN 
		END IF	
	ELSE
		IF istr_mant.argumento[12] = "" THEN			  
			dw_1.Object.lfcd_tipool[il_fila] =  1
			istr_mant.argumento[12] = "1"
		ELSEIF istr_mant.argumento[12] = "1" THEN
			dw_1.Object.lfcd_tipool[il_fila] =  1
		ELSE
			MessageBox("Error de Tipo de Pool","Ingreso de Pool Distinto al Inicial (Venta).~r Esta Información debe ser Ingresada en Otro Lote.")
			dw_1.SetItem(il_fila, "vari_codigo", Integer(ls_Nula))
			RETURN 
		END IF	
	END IF
	
	dw_1.SetItem(il_Fila, "vari_codigo", Integer(lstr_Busq.Argum[3]))
	dw_1.SetItem(il_Fila, "vari_nombre", lstr_Busq.Argum[4])

	dw_1.SetColumn("frio_tipofr")
	dw_1.SetFocus()
ELSE
	dw_1.SetColumn("vari_codigo")
	dw_1.SetFocus()
END IF
end subroutine

public function integer retiraprod (string as_columna, string as_valor);Integer li_espe, li_vari, li_cate
Long    ll_Productor, ll_prod
Date	  ld_Fecha

ld_fecha = Date(istr_mant.argumento[11])
ll_prod  = dw_1.Object.prod_codigo[il_fila]
li_espe  = dw_1.Object.lofc_espcod[il_fila]
li_vari  = dw_1.Object.vari_codigo[il_fila]
li_cate  = dw_1.Object.cate_codigo[il_fila]

Choose Case as_columna
		
	CASE "prod_codigo"
		ll_prod = long(as_valor)
	
	CASE "vari_codigo"
		li_vari = integer(as_valor)
	
	CASE "cate_codigo"
		li_cate = Integer(as_valor)
		
END CHOOSE		

IF isnull(ll_prod) or isnull(li_vari) or isnull(li_cate) THEN
	RETURN 0
ELSE	

	SELECT   prod_codigo
	INTO		:ll_productor
	FROM		dbo.spro_pararetiroprod as prp
	WHERE		prp.prod_codigo = :ll_prod
	AND		prp.espe_codigo = :li_espe
	AND		prp.vari_codigo = :li_vari
	AND      prp.cate_codigo = :li_cate
	AND      prp.prep_fecini < :ld_fecha;
	
	IF SqlCa.SQLCode = -1 THEN
		F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_pararetiroprod")
	
		RETURN 0
	ELSEIF SqlCa.SQLCode = 100 THEN
	
		RETURN 1
	END IF
	
	RETURN 2
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

	dw_1.SetColumn("refe_gcalib")
	dw_1.SetFocus()
END IF

RETURN
end subroutine

public function decimal obtienekilosant (long al_tarja);decimal	ld_kilos, ld_tara
Long		ll_Bins, ll_Cliente, ll_lote

ll_Cliente = 	Integer(istr_Mant.Argumento[13])
ll_Bins	=		dw_1.Object.bins_numero[il_fila] 
ll_lote	=		dw_1.Object.lofc_lotefc[il_fila] 

SELECT sum(lfcd_kilpro)
INTO :ld_kilos
FROM dbo.spro_lotesfrutacomdeta AS lcd,
		dbo.spro_movtobins as mvb,
		dbo.spro_bins as bin
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
FROM dbo.spro_calicosechero as cal,
		dbo.spro_bins as bin
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

public function boolean existetarja (integer ai_cliente, integer ai_planta, long al_tarja);Long	 	ll_fila
Boolean	lb_Retorno

ll_fila = dw_1.Find("fgmb_nrotar = " + String(al_tarja) , 1, dw_1.RowCount())		

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error", "La tarja digitada a pertenece a este Proceso.", Exclamation!)
	lb_Retorno	=	TRUE
END IF


IF NOT lb_Retorno THEN
	SELECT	fgmb_nrotar
		INTO	:ll_fila
		FROM	dbo.spro_movtobins
		WHERE	clie_codigo=	:ai_cliente
		AND	plde_codigo	=	:ai_planta
		AND  	fgmb_nrotar	=	:al_tarja;	
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla Spro_Bins")
	
		lb_Retorno	=	TRUE
	ELSEIF sqlca.SQLCode = 100 THEN
		lb_Retorno	=	FALSE
	ELSE
		MessageBox("Error", "La tarja digitada a pertenece a otro Proceso.", Exclamation!)
		lb_Retorno	= TRUE
	END IF
END IF

RETURN lb_Retorno
end function

on w_mant_deta_lotesfrutacomdeta.create
int iCurrent
call super::create
this.ole_puerta=create ole_puerta
this.dw_9=create dw_9
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.ole_puerta
this.Control[iCurrent+2]=this.dw_9
end on

on w_mant_deta_lotesfrutacomdeta.destroy
call super::destroy
destroy(this.ole_puerta)
destroy(this.dw_9)
end on

event open;/*

	Argumentos
		istr_Mant.Argumento[1]	=	Código Planta
		istr_Mant.Argumento[2]	=	Tipo de Movimiento
		istr_Mant.Argumento[3]	=	Número de Movimiento
		istr_Mant.Argumento[4]	=	Sentido del Movimiento
											1 => Recepcion, 2 => Despacho
		istr_Mant.Argumento[13]	=	Cliente
		istr_Mant.Argumento[15]	=	Variedad	
*/
Integer 	li_resultado
String		ls_parametros
Boolean	ib_OCX	=	True

x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

iuo_PltaLote			=	Create uo_plantadesp
iuo_Especie			=	Create uo_especie
iuo_LoteFC			=	Create uo_lotesfrutacomer
iuo_Productor		=	Create uo_productores
iuo_Camara			=	Create uo_camarasfrigo
iuo_TipoMovto		=	Create uo_tipomovtofruta
iuo_TipoFrio			=	Create uo_tratamientofrio
iuo_PeriodoFrio		=	Create uo_periodofrio
iuo_Predio			=	Create uo_predios
iuo_cuartel			=	Create uo_prodcuarteles
iuo_categoria		=	Create uo_categorias
iuo_calidad			=	Create uo_calicosechero
iuo_bins				=	Create uo_bins

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

dw_1.GetChild("frio_tipofr", idwc_TipoFrio)
idwc_TipoFrio.SetTransObject(sqlca)
idwc_TipoFrio.Retrieve()
idwc_TipoFrio.SetSort("frio_nombre A")
idwc_TipoFrio.Sort()

dw_1.GetChild("pefr_codigo", idwc_PeriodoFrio)
idwc_PeriodoFrio.SetTransObject(sqlca)
idwc_PeriodoFrio.Retrieve()
idwc_PeriodoFrio.SetSort("pefr_nombre A")
idwc_PeriodoFrio.Sort()

dw_1.GetChild("enva_tipoen", idwc_TipoEnvase)
idwc_TipoEnvase.SetTransObject(sqlca)
idwc_TipoEnvase.Retrieve()
idwc_TipoEnvase.SetSort("tien_nombre A")
idwc_TipoEnvase.Sort()
idwc_TipoEnvase.SetFilter("tien_usoenv = 1")
idwc_TipoEnvase.Filter()

dw_1.GetChild("bins_numero", idwc_bins)
idwc_bins.SetTransObject(sqlca)
idwc_bins.Retrieve(Integer(istr_Mant.Argumento[13]),Integer(istr_Mant.Argumento[1]),0)

dw_1.GetChild("prbr_codpre",idwc_predio)
idwc_predio.SetTransObject(SQLCA)
IF idwc_predio.Retrieve(-1) = 0 THEN
	idwc_predio.InsertRow(0)
END IF

dw_1.GetChild("prcc_codigo",idwc_cuartel)
idwc_cuartel.SetTransObject(SQLCA)
IF idwc_cuartel.Retrieve(-1,-1) = 0 THEN
	idwc_cuartel.InsertRow(0)
END IF

dw_1.GetChild("cate_codigo",idwc_categoria)
idwc_categoria.SetTransObject(SQLCA)
IF idwc_categoria.Retrieve(-1) = 0 THEN
	idwc_categoria.InsertRow(0)
END IF

idwc_categoria.SetFilter("cate_embala <> 1")
idwc_categoria.Filter()

dw_1.GetChild("cale_calida", idwc_calidad)
idwc_calidad.SetTransObject(sqlca)

dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)

il_fila			=	dw_1.GetRow()
li_resultado 	=	ConfiguracionPuerta(istr_puertacomm)

IF li_resultado = 0 THEN
	ls_parametros	=	String(istr_puertacomm.Baudios)+","+istr_puertacomm.Paridad+","+&
							String(istr_puertacomm.Data)+","+String(istr_puertacomm.Parada)
			
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

event ue_antesguardar();Integer	li_Contador
String	ls_Mensaje, ls_Columna[]
Date	ld_Fecha

IF IsNull(dw_1.Object.lfcd_nturno[il_Fila]) THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nNúmero de Turno del Turno"
	ls_Columna[li_Contador]	=	"lfcd_nturno"
END IF

IF IsNull(dw_1.Object.prod_codigo[il_Fila]) OR &
	dw_1.Object.prod_codigo[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nProductor del Lote"
	ls_Columna[li_Contador]	=	"prod_codigo"
END IF

IF IsNull(dw_1.Object.vari_codigo[il_Fila]) OR &
	dw_1.Object.vari_codigo[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nVariedad del Lote"
	ls_Columna[li_Contador]	=	"vari_codigo"
END IF

IF IsNull(dw_1.Object.frio_tipofr[il_Fila]) OR &
	dw_1.Object.frio_tipofr[il_Fila] = "" THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nTipo Frio del Lote"
	ls_Columna[li_Contador]	=	"frio_tipofr"
END IF

IF IsNull(dw_1.Object.pefr_codigo[il_Fila]) OR &
	dw_1.Object.pefr_codigo[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nPeriodo Frio del Lote"
	ls_Columna[li_Contador]	=	"pefr_codigo"
END IF

IF IsNull(dw_1.Object.enva_tipoen[il_Fila]) OR &
	dw_1.Object.enva_tipoen[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nTipo Envase del Lote"
	ls_Columna[li_Contador]	=	"enva_tipoen"
END IF

IF IsNull(dw_1.Object.enva_codigo[il_Fila]) OR &
	dw_1.Object.enva_codigo[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nEnvase del Lote"
	ls_Columna[li_Contador]	=	"enva_codigo"
END IF

IF IsNull(dw_1.Object.cate_codigo[il_Fila]) OR &
	dw_1.Object.cate_codigo[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCategoria del Lote"
	ls_Columna[li_Contador]	=	"cate_codigo"
END IF

IF IsNull(dw_1.Object.refe_gcalib[il_Fila]) OR &
	dw_1.Object.refe_gcalib[il_Fila] = "" THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nGrupo Calibre del Lote"
	ls_Columna[li_Contador]	=	"refe_gcalib"
END IF

IF IsNull(dw_1.Object.lfcd_bultos[il_Fila]) OR &
	dw_1.Object.lfcd_bultos[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCantidad de Bultos"
	ls_Columna[li_Contador]	=	"lfcd_bultos"
END IF

IF IsNull(dw_1.Object.lfcd_kilnet[il_Fila]) OR &
	dw_1.Object.lfcd_kilnet[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nKilos Netos"
	ls_Columna[li_Contador]	=	"lfcd_kilnet"
END IF

IF li_Contador > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_Columna[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

event ue_deshace;call super::ue_deshace;IF UpperBound(ias_campo) > 0 THEN
	dw_1.Object.lfcd_nturno[il_fila]	=	Integer(ias_campo[1])
	dw_1.Object.prod_codigo[il_fila]	=	long(ias_campo[2])
	dw_1.Object.vari_codigo[il_fila]	=	Integer(ias_campo[3])
	dw_1.Object.frio_tipofr[il_fila]	=	ias_campo[4]
	dw_1.Object.pefr_codigo[il_fila]	=	Integer(ias_campo[5])
	dw_1.Object.enva_tipoen[il_fila]	=	Integer(ias_campo[6])
	dw_1.Object.enva_codigo[il_fila]	=	Integer(ias_campo[7])
	dw_1.Object.refe_calibr[il_fila]	=	ias_campo[8]
	dw_1.Object.refe_gcalib[il_fila]	=	ias_campo[9]
	dw_1.Object.lfcd_bultos[il_fila]	=	Dec(ias_campo[10])
	dw_1.Object.lfcd_kilnet[il_fila]	=	Dec(ias_campo[11])
	dw_1.Object.prbr_codpre[il_fila]	=	integer(ias_campo[12])
	dw_1.Object.prcc_codigo[il_fila]	=	integer(ias_campo[13])
	dw_1.Object.cate_codigo[il_fila]	=	integer(ias_campo[14])
END IF
end event

event ue_nuevo;call super::ue_nuevo;dw_1.Object.lofc_pltcod[il_fila]		=	gstr_ParamPlanta.CodigoPlanta
dw_1.Object.lofc_espcod[il_fila]	=	Integer(istr_Mant.Argumento[7])
dw_1.Object.lofc_lotefc[il_fila]		=	Integer(istr_Mant.Argumento[6])
dw_1.Object.prod_codigo[il_Fila]	=	Long(istr_Mant.Argumento[10])
dw_1.Object.vari_codigo[il_Fila]	=	Integer(istr_Mant.Argumento[15])

ExisteVariedad(Integer(istr_Mant.Argumento[7]), Integer(istr_Mant.Argumento[15]), istr_Variedad)
dw_1.Object.vari_nombre[il_Fila]	=	istr_Variedad.Nombre

iuo_Productor.Existe(Long(istr_Mant.Argumento[10]), False, SqlCa)
dw_1.Object.prod_nombre[il_Fila]	=	iuo_Productor.Nombre

dw_1.GetChild("prbr_codpre",idwc_predio)
idwc_predio.SetTransObject(SQLCA)
IF idwc_predio.Retrieve(long(istr_Mant.Argumento[10])) = 0 THEN
	idwc_predio.InsertRow(0)
END IF

iuo_PltaLote.Existe(gstr_ParamPlanta.CodigoPlanta, True, SqlCa)
iuo_Especie.Existe(Integer(istr_Mant.Argumento[7]), True, SqlCa)
end event

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1]	=	String(dw_1.Object.lfcd_nturno[il_Fila])
ias_campo[2]	=	String(dw_1.Object.prod_codigo[il_Fila])
ias_campo[3]	=	String(dw_1.Object.vari_codigo[il_Fila])
ias_campo[4]	=	dw_1.Object.frio_tipofr[il_Fila]
ias_campo[5]	=	String(dw_1.Object.pefr_codigo[il_Fila])
ias_campo[6]	=	String(dw_1.Object.enva_tipoen[il_Fila])
ias_campo[7]	=	String(dw_1.Object.enva_codigo[il_Fila])
ias_campo[8]	=	dw_1.Object.refe_calibr[il_Fila]
ias_campo[9]	=	String(dw_1.Object.refe_gcalib[il_Fila])
ias_campo[10]	=	String(dw_1.Object.lfcd_bultos[il_Fila])
ias_campo[11]	=	String(dw_1.Object.lfcd_kilnet[il_Fila])
ias_campo[12]	=	String(dw_1.Object.prbr_codpre[il_Fila])
ias_campo[13]	=	String(dw_1.Object.prcc_codigo[il_Fila])
ias_campo[14]	=	String(dw_1.Object.cate_codigo[il_Fila])


dw_1.GetChild("prbr_codpre",idwc_predio)
idwc_predio.SetTransObject(SQLCA)
IF idwc_predio.Retrieve(long(ias_campo[2])) = 0 THEN
	idwc_predio.InsertRow(0)
END IF

dw_1.GetChild("prcc_codigo",idwc_cuartel)
idwc_cuartel.SetTransObject(SQLCA)
IF idwc_cuartel.Retrieve(long(ias_campo[2]),integer(ias_campo[12])) = 0 THEN
	idwc_cuartel.InsertRow(0)
END IF
	
IF istr_mant.Agrega THEN
	dw_1.Object.lofc_pltcod[il_fila]	=	gstr_ParamPlanta.CodigoPlanta
	dw_1.Object.lofc_espcod[il_fila]	=	Integer(istr_Mant.Argumento[7])
	dw_1.Object.lofc_lotefc[il_fila]	=	Integer(istr_Mant.Argumento[6])
	dw_1.Object.prod_codigo[il_Fila]	=	Long(istr_Mant.Argumento[10])
	dw_1.Object.vari_codigo[il_fila]	=	Integer(istr_Mant.Argumento[15])
	
	ExisteVariedad(Integer(istr_Mant.Argumento[7]), Integer(istr_Mant.Argumento[15]), istr_Variedad)
	dw_1.Object.vari_nombre[il_Fila]	=	istr_Variedad.Nombre
			
	iuo_Productor.Existe(Long(istr_Mant.Argumento[10]), False, SqlCa)
	dw_1.Object.prod_nombre[il_Fila]	=	iuo_Productor.Nombre

	dw_1.GetChild("prbr_codpre",idwc_predio)
	idwc_predio.SetTransObject(SQLCA)
	IF idwc_predio.Retrieve(long(istr_Mant.Argumento[10])) = 0 THEN
		idwc_predio.InsertRow(0)
	END IF
	
	iuo_PltaLote.Existe(gstr_ParamPlanta.CodigoPlanta, True, sqlca)
	iuo_Especie.Existe(Integer(istr_Mant.Argumento[7]), True, sqlca)
END IF
end event

event timer;call super::timer;//Integer	li_factor, li_posini, li_LarBuf
//String 	ls_string
//Double	ld_kilos
//
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
//	dw_1.Object.Kilos[1]	=	String(ld_kilos)
//END IF
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_lotesfrutacomdeta
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_lotesfrutacomdeta
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_lotesfrutacomdeta
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_lotesfrutacomdeta
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_lotesfrutacomdeta
integer x = 2400
integer y = 320
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_lotesfrutacomdeta
integer x = 2400
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_lotesfrutacomdeta
integer x = 2400
integer y = 516
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_lotesfrutacomdeta
integer x = 82
integer y = 112
integer width = 2103
integer height = 1716
string dataobject = "dw_mant_lotesfrutacomdeta"
end type

event dw_1::buttonclicked;call super::buttonclicked;Long 		ll_Fila
Dec{2}	ld_Bultos
Dec{3}	ld_KNetos
decimal	ldec_kilosant

CHOOSE CASE dwo.Name
	CASE "b_productor"
		BuscaProductor()

	CASE "b_variedad"
		BuscaVariedad()

	CASE "b_envase"
		BuscaEnvase()
		
	CASE "b_romana"
		IF gstr_paramplanta.binsabins OR gstr_paramplanta.bultobins THEN
			
			IF (istr_puertacomm.pesajebins = 1 AND Dec(dw_1.Object.Kilos[row]) >= istr_puertacomm.PesoMinimo) OR &
				(Dec(dw_1.Object.Kilos[row]) >= 0) THEN	
				IF This.Object.bins_numero[il_Fila] > 0 THEN
					
					IF dw_1.Object.Kilos[row] > 0 THEN
						This.Object.lfcd_kilbru[row]	=	dw_1.Object.Kilos[row]
					ELSE
						dw_1.Object.Kilos[1]				=	This.Object.lfcd_kilbru[row]
					END IF
					
					ld_Bultos								=	This.Object.lfcd_bultos[row]
					
					IF ld_Bultos = 0 OR IsNull(ld_Bultos) THEN ld_Bultos = 1
					
					ldec_kilosant 							= 	ObtieneKilosant(This.Object.fgmb_nrotar[row]) * ld_Bultos
					This.Object.lfcd_kilnet[il_Fila]	=	Round((This.Object.lfcd_kilbru[row] - ldec_kilosant), 3)
					This.Object.lfcd_kilpro[il_Fila]	=	Round((This.Object.lfcd_kilnet[il_Fila] / ld_Bultos) , 3)
					dw_1.Object.Kilos[row]				=	0
				ELSE
					MessageBox("Atención", "Debe Ingresar Número de Bins Previamente")
					Return
				END IF
			END IF
		ELSE
			wstr_pesaje.puerta		=	istr_puertacomm
			wstr_pesaje.dw				=	dw_9
			
			wstr_pesaje.argum[1]		=	istr_mant.argumento[1]
			wstr_pesaje.argum[2]		=	istr_mant.argumento[2]
			wstr_pesaje.argum[3]		=	istr_mant.argumento[3]
			wstr_pesaje.Argum[7]		=	Istr_Mant.Argumento[6]			
			wstr_pesaje.argum[10]	=	istr_Mant.Argumento[13]
			
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

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna, ls_Fecha, ls_Nula
Integer	li_retiraprod, li_estado

SetNull(ls_Nula)

ls_Columna = dwo.Name

CHOOSE CASE ls_Columna

	CASE "prod_codigo"
		
		This.Object.prbr_codpre[il_Fila]	=	Integer(ls_Nula)
		This.Object.prcc_codigo[il_Fila]	=	Integer(ls_Nula)
		
		IF iuo_Productor.Existe(long(Data),True,SqlCa) THEN
			This.Object.prod_nombre[il_Fila]	=	iuo_Productor.Nombre
			
			dw_1.GetChild("prbr_codpre",idwc_predio)
			idwc_predio.SetTransObject(SQLCA)
			IF idwc_predio.Retrieve(Long(data)) = 0 THEN
				idwc_predio.InsertRow(0)
			END IF
			idwc_cuartel.Reset()
	      //llama la funcion retiraprod, entregando los argumentos
			//y evaluando si retira el Prod. para asignar valor a "lofc_Tipool"
			li_retiraprod	=	retiraprod("prod_codigo",data)
			IF li_retiraprod = 2 THEN
							  
				IF istr_mant.argumento[12] = "" THEN
		  			dw_1.Object.lfcd_tipool[il_fila] =  2
					istr_mant.argumento[12] = "2"
				ELSEIF istr_mant.argumento[12] = "2" THEN
					dw_1.Object.lfcd_tipool[il_fila] =  2
				ELSE
					MessageBox("Error de Tipo de Pool","Ingreso de Pool Distinto al Inicial (Retiro).~r Esta Información debe ser Ingresada en Otro Lote.")
					This.SetItem(il_fila, ls_Columna, Integer(ls_Nula))
					RETURN 1
				END IF	
			ELSEIF li_retiraprod <> 0 THEN
				IF istr_mant.argumento[12] = "" THEN
					dw_1.Object.lfcd_tipool[il_fila] =  1
					istr_mant.argumento[12] = "1"
				ELSEIF istr_mant.argumento[12] = "1" THEN
					dw_1.Object.lfcd_tipool[il_fila] =  1
				ELSE
					MessageBox("Error de Tipo de Pool","Ingreso de Pool Distinto al Inicial (Venta).~r Esta Información debe ser Ingresada en Otro Lote.")
					This.SetItem(il_fila, ls_Columna, Integer(ls_Nula))
					RETURN 1
				END IF	
			END IF    
		ELSE
			This.Object.prod_codigo[il_Fila]	=	Integer(ls_Nula)
			This.Object.prod_nombre[il_Fila]	=	ls_Nula
			idwc_Cuartel.Reset()
			idwc_predio.Reset()
			RETURN 1
		END IF
	
	CASE "prbr_codpre"
		This.Object.prcc_codigo[il_Fila]	=	Integer(ls_Nula)
		
		IF iuo_Predio.Existe(SQLCA, TRUE, dw_1.Object.prod_codigo[il_Fila],&
		                     integer(data)) THEN
			dw_1.GetChild("prcc_codigo",idwc_cuartel)
			idwc_Cuartel.SetTransObject(SQLCA)
			IF idwc_Cuartel.Retrieve(dw_1.Object.prod_codigo[il_Fila],Integer(data)) = 0 THEN
				idwc_Cuartel.InsertRow(0)
			END IF
		ELSE
			This.Object.prbr_codpre[il_Fila]	=	Integer(ls_Nula)
			idwc_Cuartel.Reset()
			RETURN 1
		END IF

	CASE "prcc_codigo"
		IF Not iuo_cuartel.Existe(dw_1.Object.prod_codigo[il_Fila], dw_1.Object.prbr_codpre[il_Fila], &
		       integer(data), True, SQLCA) THEN
			This.Object.prcc_codigo[il_Fila]	=	Integer(ls_Nula)
			RETURN 1
		END IF
		
		
	CASE "vari_codigo"
		IF ExisteVariedad(Integer(istr_Mant.Argumento[7]), &
								Integer(Data),istr_Variedad) THEN
			This.SetItem(il_Fila, "vari_nombre", istr_Variedad.Nombre)
	      //llama la funcion retiraprod, entregando los argumentos
			//y evaluando si retira el Prod. para asignar valor a "lofc_Tipool"
			li_retiraprod	=	retiraprod("prod_codigo",data)
			IF li_retiraprod = 2 THEN
							  
				IF istr_mant.argumento[12] = "" THEN			  
		  			dw_1.Object.lfcd_tipool[il_fila] =  2
					istr_mant.argumento[12] = "2"
				ELSEIF istr_mant.argumento[12] = "2" THEN
					dw_1.Object.lfcd_tipool[il_fila] =  2
				ELSE
					MessageBox("Error de Tipo de Pool","Ingreso de Pool Distinto al Inicial (Retiro).~r Esta Información debe ser Ingresada en Otro Lote.")
					This.SetItem(il_fila, ls_Columna, Integer(ls_Nula))
					RETURN 1
				END IF	
			ELSEIF li_retiraprod <> 0 THEN
				IF istr_mant.argumento[12] = "" THEN			  
					dw_1.Object.lfcd_tipool[il_fila] =  1
					istr_mant.argumento[12] = "1"
				ELSEIF istr_mant.argumento[12] = "1" THEN
					dw_1.Object.lfcd_tipool[il_fila] =  1
				ELSE
					MessageBox("Error de Tipo de Pool","Ingreso de Pool Distinto al Inicial (Venta).~r Esta Información debe ser Ingresada en Otro Lote.")
					This.SetItem(il_fila, ls_Columna, Integer(ls_Nula))
					RETURN 1
				END IF	
			END IF    

		ELSE
			This.Object.vari_codigo[il_Fila]	=	Integer(ls_Nula)
			This.Object.vari_nombre[il_Fila]	=	ls_Nula
			RETURN 1
		END IF

	CASE "frio_tipofr"
		IF NOT iuo_TipoFrio.ofp_recupera_tratamientofrio(SqlCa,Data,True) THEN
			This.Object.frio_tipofr[il_Fila]	=	ls_Nula
			RETURN 1
		END IF

	CASE "pefr_codigo"
		IF NOT iuo_PeriodoFrio.ofp_recupera_periodofrio(SqlCa,Integer(Data),True) THEN
			This.Object.pefr_codigo[il_Fila]	=	Integer(ls_Nula)
			RETURN 1
		END IF

	CASE "enva_tipoen"
		IF NOT ExisteEnvase(Integer(Data), 0, istr_Envase) OR &
			istr_Envase.UsoEnvase <> 1 THEN
			This.Object.enva_tipoen[il_Fila]	=	Integer(ls_Nula)

			RETURN 1
		ELSE
			idwc_calidad.Retrieve(Integer(data), This.Object.enva_codigo[row])
		END IF

	CASE "enva_codigo"
		IF NOT ExisteEnvase(istr_Envase.TipoEnvase, Integer(Data), istr_Envase) THEN
			This.Object.enva_codigo[il_Fila]	=	Integer(ls_Nula)
			This.Object.enva_nombre[il_Fila]	=	ls_Nula

			RETURN 1
		ELSE
			idwc_calidad.Retrieve(This.Object.enva_tipoen[row], Integer(data))
			This.Object.enva_nombre[il_Fila]	=	istr_Envase.Nombre
		END IF
		
	CASE "cale_calida"
		IF NOT iuo_calidad.Existe(This.Object.enva_tipoen[row], This.Object.enva_codigo[row], &
									  data, True, Sqlca) THEN
			This.Object.cale_calida[il_Fila] = ls_Nula
			
			Return 1
		END IF
	CASE "cate_codigo"
		IF Not iuo_Categoria.Existe(integer(data), True,SQLCA) THEN
			This.Object.cate_codigo[il_Fila]	=	Integer(ls_Nula)
			RETURN 1
		ELSE
//			//llama la funcion retiraprod, entregando los argumentos
//			//y evaluando si retira el Prod. para asignar valor a "lofc_Tipool"
//			li_retiraprod	=	retiraprod("prod_codigo",data)
//			IF li_retiraprod = 2 THEN
//							  
//				IF istr_mant.argumento[12] = "" THEN			  
//		  			dw_1.Object.lfcd_tipool[il_fila] =  2
//					istr_mant.argumento[12] = "2"
//				ELSEIF istr_mant.argumento[12] = "2" THEN
//					dw_1.Object.lfcd_tipool[il_fila] =  2
//				ELSE
//					MessageBox("Error de Tipo de Pool","Ingreso de Pool Distinto al Inicial (Retiro).~r Esta Información debe ser Ingresada en Otro Lote.")
//					This.SetItem(il_fila, ls_Columna, Integer(ls_Nula))
//					RETURN 1
//				END IF	
//			ELSEIF li_retiraprod <> 0 THEN
//				IF istr_mant.argumento[12] = "" THEN			  
//					dw_1.Object.lfcd_tipool[il_fila] =  1
//					istr_mant.argumento[12] = "1"
//				ELSEIF istr_mant.argumento[12] = "1" THEN
//					dw_1.Object.lfcd_tipool[il_fila] =  1
//				ELSE
//					MessageBox("Error de Tipo de Pool","Ingreso de Pool Distinto al Inicial (Venta).~r Esta Información debe ser Ingresada en Otro Lote.")
//					This.Object.cate_codigo[row]	=	Integer(ls_Nula)
//					RETURN 1
//				END IF	
//			END IF    

		END IF

	CASE "lfcd_bultos"
		Calcula(Data, 1)

	CASE "lfcd_kilnet"
		Calcula(Data, 2)
		
	CASE "fgmb_nrotar"
		IF ExisteTarja(Integer(istr_mant.argumento[13]), dw_1.Object.lofc_pltcod[1], Long(data)) THEN
			This.Object.fgmb_nrotar[row]	=	Long(ls_Nula)
			This.SetColumn(ls_columna)
			This.SetFocus()
			RETURN 1
		END IF
		
	CASE "bins_numero"
		IF NOT iuo_bins.Existe(Integer(istr_mant.argumento[13]), dw_1.Object.lofc_pltcod[1], Long(data), sqlca, TRUE) THEN
			This.SetItem(row, ls_columna, Long(ls_Nula))
			This.SetColumn(ls_columna)
			This.SetFocus()
			RETURN 1
		ELSE
			li_estado	=	iuo_bins.Estado(Integer(istr_mant.argumento[13]), dw_1.Object.lofc_pltcod[1],&
												 dw_1.Object.fgmb_nrotar[row], Long(data), sqlca)
			
			IF li_estado = 1 OR li_estado < 0 THEN
				MessageBox("Error", "El estado del bins no permite asociarlo a otro movimiento")
				This.SetItem(row, ls_columna, Long(ls_Nula))
				This.SetColumn(ls_columna)
				This.SetFocus()
				RETURN 1
			ELSE
				THIS.Object.Enva_tipoen[row]	=	iuo_bins.enva_tipoen
				THIS.Object.Enva_codigo[row]	=	iuo_bins.enva_codigo
				THIS.Object.enva_nombre[row]	=	iuo_bins.enva_nombre
				
				idwc_calidad.Retrieve(iuo_bins.enva_tipoen, iuo_bins.enva_codigo)
				THIS.Object.cale_calida[row]	=	iuo_bins.cale_calida
			END IF
		END IF
		
END CHOOSE
end event

type ole_puerta from olecustomcontrol within w_mant_deta_lotesfrutacomdeta
event oncomm ( )
boolean visible = false
integer x = 2391
integer y = 680
integer width = 174
integer height = 152
integer taborder = 50
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
string binarykey = "w_mant_deta_lotesfrutacomdeta.win"
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
end type

type dw_9 from datawindow within w_mant_deta_lotesfrutacomdeta
boolean visible = false
integer width = 78
integer height = 72
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dw_pesaje_romana"
boolean hscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
0Fw_mant_deta_lotesfrutacomdeta.bin 
2B00000600e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe00000006000000000000000000000001000000010000000000001000fffffffe00000000fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1Fw_mant_deta_lotesfrutacomdeta.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
