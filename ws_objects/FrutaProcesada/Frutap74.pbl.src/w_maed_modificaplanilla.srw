$PBExportHeader$w_maed_modificaplanilla.srw
forward
global type w_maed_modificaplanilla from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_modificaplanilla
end type
type dw_4 from datawindow within w_maed_modificaplanilla
end type
type dw_5 from datawindow within w_maed_modificaplanilla
end type
type dw_6 from datawindow within w_maed_modificaplanilla
end type
type dw_7 from datawindow within w_maed_modificaplanilla
end type
type dw_8 from datawindow within w_maed_modificaplanilla
end type
type dw_9 from datawindow within w_maed_modificaplanilla
end type
type dw_10 from datawindow within w_maed_modificaplanilla
end type
type dw_11 from datawindow within w_maed_modificaplanilla
end type
type dw_12 from datawindow within w_maed_modificaplanilla
end type
type dw_13 from datawindow within w_maed_modificaplanilla
end type
type dw_14 from datawindow within w_maed_modificaplanilla
end type
type dw_15 from datawindow within w_maed_modificaplanilla
end type
type dw_alpalletencab from datawindow within w_maed_modificaplanilla
end type
type dw_alpalletfruta from datawindow within w_maed_modificaplanilla
end type
type dw_palletencahisto from datawindow within w_maed_modificaplanilla
end type
type dw_palletfrutahisto from datawindow within w_maed_modificaplanilla
end type
end forward

global type w_maed_modificaplanilla from w_mant_encab_deta_csd
integer width = 3086
integer height = 2324
string title = "MANTENCIÓN  DESPACHO SAG"
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
event ue_imprimir ( )
dw_3 dw_3
dw_4 dw_4
dw_5 dw_5
dw_6 dw_6
dw_7 dw_7
dw_8 dw_8
dw_9 dw_9
dw_10 dw_10
dw_11 dw_11
dw_12 dw_12
dw_13 dw_13
dw_14 dw_14
dw_15 dw_15
dw_alpalletencab dw_alpalletencab
dw_alpalletfruta dw_alpalletfruta
dw_palletencahisto dw_palletencahisto
dw_palletfrutahisto dw_palletfrutahisto
end type
global w_maed_modificaplanilla w_maed_modificaplanilla

type variables

DataWindowChild	dw_planta, dw_plades, dw_puerto,dw_etiquetas,dw_plantas,dw_clientes,&
						dw_ptodes,dw_sitios,idwc_patente,idwc_multipuerto,idwc_especie,idwc_cargotecnico,&
						idwc_ingrediente,idwc_tratamiento,idwc_inspector

Transaction	sqlconec
Transaction	sqlconec2

Boolean		ib_conectado, ib_existe_folioD, ib_conectado2
Integer     ii_tipoin, ii_secuen, ii_controlaaceso, ii_existe, ii_blockcont, il_destino, il_desreferencia, il_existepal, il_control, il_cierra
Long        il_numins, il_Folio
String		is_rutclie

DataStore	ids_palletfruta_fecha, ids_CorrelMovim

uo_patente				iuo_patente
uo_especie				iuo_especie
uo_clienprove			iuo_clprv
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso (string columna)
public function long buscanuevofolio (integer planta)
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine existe_cargaregistro ()
public function boolean existefolio (string as_columna, string as_valor)
public function boolean buscaregistros ()
public subroutine enviamail ()
public function boolean existe_pallet (integer ai_cliente, integer ai_planta, long al_pallet)
public subroutine buscacliente ()
public function boolean existe_plasag (integer ai_planta, long ai_plasag)
public function boolean duplicado (string as_valor)
public function boolean existeubicacionsello (string as_valor)
public function boolean planillaanulada (integer ai_planta, integer ai_plasag, string ai_tipoplani)
public function boolean existe_tratamiento (string as_valor)
public function boolean existe_ingredienteactivo (string as_valor)
public function boolean existe_concentracion (string as_valor)
public function boolean existe_duracion (string as_valor)
public function boolean existe_temperatura (long al_valor)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "DESPACHO DE FRUTA PROCESADA"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_despafrigoen"

vinf.dw_1.GetChild("plde_codigo", dw_plantas)
vinf.dw_1.GetChild("etiq_codigo", dw_etiquetas)
vinf.dw_1.GetChild("clie_codigo", dw_clientes)

dw_plantas.SetTransObject(sqlca)
dw_etiquetas.SetTransObject(sqlca)
dw_clientes.SetTransObject(sqlca)

dw_plantas.Retrieve(1)
dw_etiquetas.Retrieve()
dw_clientes.Retrieve()

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]),Long(istr_mant.argumento[2]))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.plde_codigo.Protect	=	0
	dw_2.Object.defe_numero.Protect	=	0

	dw_2.Object.defe_numero.Color	= 0
	dw_2.Object.plde_codigo.Color 	= 0

	dw_2.Object.defe_numero.BackGround.Color	= Rgb(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color 	= Rgb(255,255,255)
	
	dw_2.SetColumn("defe_numero")
	dw_2.SetFocus()
ELSE
	dw_2.Object.defe_numero.Protect	=	1
	dw_2.Object.plde_codigo.Protect	=	1
	
	dw_2.Object.defe_numero.Color	= Rgb(255,255,255)
	dw_2.Object.plde_codigo.Color 	= Rgb(255,255,255)
	
	dw_2.Object.defe_numero.BackGround.Color	= 553648127
	dw_2.Object.plde_codigo.BackGround.Color 	= 553648127
END IF
end subroutine

public subroutine habilitaingreso (string columna);Date		ld_fecha
Boolean	lb_estado = True

dw_2.AcceptText()

IF dw_2.RowCount() > 0 THEN
	//IsNull(dw_2.Object.defe_numero[1]) OR dw_2.Object.defe_numero[1] = 0 OR &
	IF IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &		
		IsNull(dw_2.Object.defe_fecdes[1]) OR dw_2.Object.defe_fecdes[1] = ld_fecha OR &
		IsNull(dw_2.Object.defe_cancaj[1]) OR dw_2.Object.defe_cancaj[1] = 0 OR &
		IsNull(dw_2.Object.defe_cantar[1]) OR dw_2.Object.defe_cantar[1] = 0 OR &
		IsNull(dw_2.Object.defe_guides[1]) OR dw_2.Object.defe_guides[1] = 0 OR &	
		IsNull(dw_2.Object.defe_especi[1]) OR dw_2.Object.defe_especi[1] = 0 THEN
		lb_estado = False			
	END IF
END IF

IF ii_blockcont = 1 THEN
	IF IsNull(dw_2.Object.defe_nrcont[1]) OR dw_2.Object.defe_nrcont[1] = '' OR &		
		IsNull(dw_2.Object.defe_orcont[1]) OR dw_2.Object.defe_orcont[1] = 0 THEN
		lb_estado = False
   END IF
END IF
	
IF dw_2.Object.defe_plasag[1]  > 0 THEN
		
	IF IsNull(dw_2.Object.defe_nturno[1]) OR dw_2.Object.defe_nturno[1] = '' OR &
		   IsNull(dw_2.Object.defe_glosag[1]) OR dw_2.Object.defe_glosag[1] = '' OR &
	      IsNull(dw_2.Object.defe_tecnic[1]) OR dw_2.Object.defe_tecnic[1] = 0  OR &
			IsNull(dw_2.Object.trat_codigo[1]) OR dw_2.Object.trat_codigo[1] = 0 OR &
			IsNull(dw_2.Object.defe_fectra[1]) OR dw_2.Object.defe_fectra[1] = ld_fecha OR &
			IsNull(dw_2.Object.defe_espmul[1]) OR dw_2.Object.defe_espmul[1] = 0  THEN
			lb_estado = False
	END IF
ELSE	
	lb_estado = True
END IF

IF dw_2.Object.tica_codigo[1] = 2 OR dw_2.Object.tica_codigo[1] = 3 THEN
		
	IF IsNull(dw_2.Object.tran_codigo[1]) OR dw_2.Object.tran_codigo[1] = 0 OR &
		   IsNull(dw_2.Object.defe_chofer[1]) OR dw_2.Object.defe_chofer[1] = '' OR &
			IsNull(dw_2.Object.defe_chfrut[1]) OR dw_2.Object.defe_chfrut[1] = '' OR &
			IsNull(dw_2.Object.defe_patent[1]) OR dw_2.Object.defe_patent[1] = '' OR &			
	      IsNull(dw_2.Object.defe_cansel[1]) OR dw_2.Object.defe_cansel[1] = 0  OR &
			IsNull(dw_2.Object.defe_ubisel[1]) OR dw_2.Object.defe_ubisel[1] = '' OR &
			IsNull(dw_2.Object.defe_numsel[1]) OR dw_2.Object.defe_numsel[1] = ''  THEN
			lb_estado = False
	END IF
END IF

IF dw_2.Object.tica_codigo[1] = 2 THEN
		
	IF IsNull(dw_2.Object.defe_nrcont[1]) OR dw_2.Object.defe_nrcont[1] = '' OR &		
	   IsNull(dw_2.Object.defe_tpcont[1]) OR dw_2.Object.defe_tpcont[1] = 0  OR &
		IsNull(dw_2.Object.defe_orcont[1]) OR dw_2.Object.defe_orcont[1] = 0 THEN
		lb_estado = False
   END IF
END IF

IF IsNull(dw_2.Object.defe_especi[1]) OR dw_2.Object.defe_especi[1] = 0 THEN
	lb_estado = False
END IF

IF istr_mant.argumento[27] = '21' OR istr_mant.argumento[27] = '16'  THEN
	IF isnull(dw_2.Object.clpr_rut[1]) OR dw_2.Object.clpr_rut[1] = '' OR &
		isnull(dw_2.Object.embq_codigo[1]) OR dw_2.Object.embq_codigo[1] = '' THEN
		lb_estado = False
	END IF	
END IF

IF istr_mant.argumento[27] = '7' OR istr_mant.argumento[27] = '8' OR istr_mant.argumento[27] = '9' THEN
	IF isnull(dw_2.Object.embq_codigo[1]) OR dw_2.Object.embq_codigo[1] = '' THEN
		lb_estado = False
	END IF	
END IF	
			
pb_grabar.Enabled		=	lb_estado
pb_ins_det.Enabled  	= 	lb_estado
pb_eli_det.Enabled  	= 	lb_estado
pb_imprimir.Enabled 	= 	lb_estado
end subroutine

public function long buscanuevofolio (integer planta);/* Busca Folio para hacer una recepción de pallet a partir de
   un despacho de interpanta
	usando conextividad*/

Integer	li_planta
Long		ll_numero
Boolean	lb_nulo

li_planta	=	planta

SELECT Max(rfpe_numero)
  INTO :ll_numero
  FROM dbo.RECFRUPROCEE_TRANS
 WHERE plde_codigo = :li_planta
 USING sqlconec;

IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla RecFruProcee_Trans")
ELSEIF sqlconec.SQLCode = 0 THEN
	
	 lb_nulo = IsNull(ll_numero)	
	
	 IF lb_nulo THEN
		 ll_numero=li_planta*10000
	 ELSE
		 ll_numero++
	 END IF
ELSE
	ll_numero=li_planta*10000
END IF

RETURN ll_numero
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update(True, False) = 1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_2.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
ELSE
	IF ids_CorrelMovim.Update()	=	1 THEN
		IF dw_2.Update(True, False) = 1 THEN
			IF dw_1.Update(True, False) = 1 THEN
				IF ids_palletfruta_fecha.Update()=1 THEN		
					Commit;
					
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
						
						RollBack;
					ELSE
						lb_Retorno	=	True
						
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
						ids_palletfruta_fecha.ResetUpdate()
						ids_CorrelMovim.ResetUpdate()
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
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine existe_cargaregistro ();Long ll_Filrec, ll_Filpal, ll_Numero, ll_Existe, ll_Pallet, ll_Filinp, ll_Numinp, ll_Secuen, ll_Totfil
Integer li_Planta, li_Cliente, li_Tipo

IF dw_2.RowCount() > 0 THEN
	
	li_Planta 	= dw_2.Object.defe_plades[1]
	ll_Numero 	= dw_2.Object.defe_numero[1]
	li_Cliente 	= dw_2.Object.clie_codigo[1]
	
	SELECT rfpe_numero  
	  INTO :ll_Existe  
	  FROM dbo.recfruprocee_trans 
	 WHERE plde_codigo = :li_Planta  AND  
			 clie_codigo = :li_Cliente AND  
			 rfpe_numero = :ll_Numero   
	USING sqlconec ;
	 
	IF ll_Existe > 0 THEN		
			DELETE FROM dbo.recfruproced_trans 
   		 WHERE plde_codigo = :li_Planta AND  
                rfpe_numero = :ll_Existe  AND  
                clie_codigo = :li_Cliente 
          USING sqlconec ;	
							
			DELETE FROM dbo.recfruprocee_trans  
			 WHERE plde_codigo = :li_Planta AND  
			       rfpe_numero = :ll_Existe  AND  
				    clie_codigo = :li_Cliente 
		   USING sqlconec ;
	END IF
 
  END IF
COMMIT ;

end subroutine

public function boolean existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe, li_cliente
Long		ll_nfolio

li_cliente	=	dw_2.Object.clie_codigo[1]
li_planta	=	dw_2.Object.plde_codigo[1]
ll_nfolio 	=	dw_2.Object.defe_numero[1]

CHOOSE CASE as_columna
	CASE "plde_codigo"
		li_planta	=	Integer(as_valor)
		
	CASE "defe_numero"
		ll_nfolio 	=	Long(as_valor)
		
END CHOOSE

SELECT	Count(*)
	INTO	:li_existe
	FROM	dbo.DESPAFRIGOEN
	WHERE	plde_codigo	=	:li_planta
	AND	defe_numero	=	:ll_nfolio 
	And   clie_codigo =  :li_cliente ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Despafrigoen")
	RETURN False
ELSEIF li_existe > 0 THEN
	istr_mant.argumento[1]	= String(li_planta)
	istr_mant.argumento[2]	= String(ll_nfolio)
	
	This.TriggerEvent("ue_recuperadatos")
	IF il_cierra = 1 THEN
		Return False
	END IF
	istr_mant.argumento[3]	= 	String(dw_2.Object.clie_codigo[1])
	istr_mant.argumento[4]	= 	String(dw_2.Object.defe_cantar[1])
	istr_mant.argumento[8]	=	String(dw_2.Object.defe_cancaj[1])
	ib_existe_folioD			=	True
   RETURN False
ELSE
	IF IsNull(ll_nfolio) THEN
		istr_mant.argumento[1]	= String(li_planta)
		istr_mant.argumento[2]	= String(ll_nfolio)
		//istr_mant.argumento[3]	= String(li_cliente)
		ib_existe_folioD			=	False
		RETURN False
	ELSE
	   MessageBox("Atención","Número de Documento No ha sido generado. Ingrese Otro.")
		ib_existe_folioD			=	False
	   RETURN True
	END IF	
END IF





end function

public function boolean buscaregistros ();Integer  li_existe, li_planta, li_cliente
Long		ll_nfolio

//li_planta = dw_2.Object.plde_codigo[1]
li_planta = dw_2.Object.defe_plades[1]
li_cliente = dw_2.Object.clie_codigo[1]
ll_nfolio  = dw_2.Object.defe_numero[1]

SELECT	Count(*)
	INTO	:ii_existe
	FROM	dbo.RECFRUPROCEE_TRANS
	WHERE	plde_codigo	=	:li_planta
	AND	rfpe_numero	=	:ll_nfolio 
	Using (sqlconec);
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla RECFRUPROCEE_TRANS")
	RETURN True
END IF	
	
IF ii_existe > 0 THEN	
	
	ll_nfolio  = dw_2.Object.defe_guides[1]
	
	SELECT	Count(*)
		INTO	:ii_existe
		FROM	dbo.RECFRUPROCEE_TRANS
		WHERE	plde_codigo	=	:li_planta
		AND	rfpe_numero	=	:ll_nfolio 
		Using (sqlconec);
		
	IF ii_existe = 0 THEN
		li_existe = 0 
		ii_existe = 1
	ELSE
		li_existe = 1
	END IF	
		
END IF

IF li_existe > 0 THEN
	RETURN True
ELSE
	RETURN False
END IF	

RETURN False


end function

public subroutine enviamail ();
end subroutine

public function boolean existe_pallet (integer ai_cliente, integer ai_planta, long al_pallet);Long		li_existe
Integer	li_cliente, li_planta, ll_pallet

SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.palletencab
	WHERE clie_codigo = :ai_cliente
	AND	plde_codigo = :ai_planta
	AND	paen_numero = :al_pallet
	Using (sqlconec);
	
	il_existepal = li_existe

IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla palletencab")
	RETURN True
END IF	

IF li_existe = 0 THEN
	SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.palletencab_trans
	WHERE clie_codigo = :ai_cliente
	AND	plde_codigo = :ai_planta
	AND	paen_numero = :al_pallet
	Using (sqlconec);
END IF	

il_existepal = li_existe

IF sqlconec.SQLCode = -1 THEN
	F_errorbasedatos(sqlconec,"Lectura tabla palletencab_trans")
	RETURN True
END IF	

IF li_existe > 0 THEN
	RETURN True
ELSE
	RETURN False
END IF	


end function

public subroutine buscacliente ();Str_busqueda	lstr_busq

lstr_busq.argum[1] = '1'

OpenWithParm(w_busc_clienprove, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF UpperBound(lstr_busq.argum) < 2 THEN Return

IF lstr_busq.argum[1] = "" THEN
//	dw_2.SetColumn("cami_patent")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clpr_rut[il_fila]		=	lstr_busq.argum[1]
	dw_2.Object.clpr_nombre[il_fila]	=	lstr_busq.argum[2]
	is_rutclie								=	lstr_busq.argum[1]
	dw_2.SetFocus()
END IF

RETURN
end subroutine

public function boolean existe_plasag (integer ai_planta, long ai_plasag);Long		li_existe, ll_numero, ll_pallet
Integer	li_cliente, li_planta, li_cont

SELECT COUNT(*),max(defe_numero),max(clie_codigo)
	INTO :li_existe,:ll_numero,:li_cliente
	FROM dbo.despafrigoen
	WHERE plde_codigo = :ai_planta
	AND	defe_plasag = :ai_plasag;



IF li_existe > 0 THEN
	
	li_cont = MessageBox("Existe", "Número de Planilla "+String(ai_plasag)+ " Ya Existe en Despacho Nro. "&
							+String(ll_numero)+" del Cliente "+string(li_cliente)+'.'+"~n~ Desea Continuar",Exclamation!, OKCancel!, 2)

	IF li_cont = 1 THEN
		RETURN False
	ELSE
		RETURN True
	END IF
ELSE	
	RETURN False
END IF	


end function

public function boolean duplicado (string as_valor);Long		ll_fila
Integer	li_codigo

ll_Fila	=	dw_1.Find("sell_numero = '" + as_valor + "'" ,1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

public function boolean existeubicacionsello (string as_valor);Long	ll_cont

SELECT count(*)
INTO :ll_cont
FROM dbo.sagubicacionsello
WHERE sell_codigo = :as_valor;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla sagubicacionsello")
	RETURN True
ELSEIF ll_cont = 0 THEN
	MessageBox("Atención","Ubicacion sello no existe. Ingrese Otro.")

	RETURN True
END IF

Return False
end function

public function boolean planillaanulada (integer ai_planta, integer ai_plasag, string ai_tipoplani);Long		li_existe, ll_numero, ll_pallet
Integer	li_cliente, li_planta, li_cont, li_tipoplani

li_tipoplani = Integer(ai_tipoplani)

SELECT COUNT(*)
	INTO :li_existe
	FROM dbo.SAGCORRELANULADOS
	WHERE plde_codigo = :ai_planta
	AND	scoa_numero = :ai_plasag
	AND	scoa_tipoco = :ai_tipoplani;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla SAGCORRELANULADOS")
	RETURN True
END IF	

IF li_existe > 0 THEN
	
	li_cont = MessageBox("Existe", "Número de Planilla "+String(ai_plasag)+ " se Encuentra Anulada Ingrese Otra. ",Exclamation!, OK!)

	RETURN True
ELSE	
	RETURN False
END IF	


end function

public function boolean existe_tratamiento (string as_valor);Long	ll_cont

SELECT count(*)
INTO :ll_cont
FROM dbo.sagtratamientoenca
WHERE tsag_numero = :as_valor;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla sagtratamientoenca")
	RETURN True
ELSEIF ll_cont = 0 THEN
	MessageBox("Atención","Tratamiento no existe. Ingrese Otro.")

	RETURN True
END IF

Return False
end function

public function boolean existe_ingredienteactivo (string as_valor);Long	ll_cont

SELECT count(*)
INTO :ll_cont
FROM dbo.Sagingredienteactivo
WHERE inac_codigo = :as_valor;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Ingrediente Activo")
	RETURN True
ELSEIF ll_cont = 0 THEN
	MessageBox("Atención","Ingrediente Activo no existe. Ingrese Otro.")

	RETURN True
END IF

Return False
end function

public function boolean existe_concentracion (string as_valor);Long	ll_cont

SELECT count(*)
INTO :ll_cont
FROM dbo.concentracion
WHERE cons_codigo = :as_valor;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla concentracion")
	RETURN True
ELSEIF ll_cont = 0 THEN
	MessageBox("Atención","concentracion no existe. Ingrese Otro.")

	RETURN True
END IF

Return False
end function

public function boolean existe_duracion (string as_valor);Long	ll_cont

SELECT count(*)
INTO :ll_cont
FROM dbo.duracion
WHERE dura_codigo = :as_valor;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla duracion")
	RETURN True
ELSEIF ll_cont = 0 THEN
	MessageBox("Atención","duracion no existe. Ingrese Otro.")

	RETURN True
END IF

Return False
end function

public function boolean existe_temperatura (long al_valor);Long	ll_cont

SELECT count(*)
INTO :ll_cont
FROM dbo.temperatura
WHERE temp_codigo = :al_valor;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla temperatura")
	RETURN True
ELSEIF ll_cont = 0 THEN
	MessageBox("Atención","temperatura no existe. Ingrese Otro.")

	RETURN True
END IF

Return False
end function

event open;x=0
y=0
/*
	Argumentos	:	[1]	=	Código de Planta
						[2]	=	Número de Folio Despacho
						[3]	=	Código de Cliente
						[4]	=	Cantidad de Tarjas
						[5]	=	Código de Embarque
						[7]	=	Código de Destino
						[8]	=	Cantidad de Cajas
						[10]	=	Especie del multipuerto
						[11]	=	Planilla Sag
						[12]	=	multipuerto
						[20]	=	Tipo movimiento
						[20]	=	Guia despacho
*/

/* Si Código de parempresa es igual a 1 se efectua la transacción para REBAJE DE EXISTENCIA*/
  SELECT empr_coacce
    INTO :ii_controlaaceso  
    FROM dbo.parempresa  
	 USING sqlca;

IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				= dw_1
istr_mant.solo_consulta = False

dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("defe_plades", dw_plades)
dw_2.GetChild("puer_codigo", dw_puerto)
dw_2.GetChild("sire_codigo", dw_sitios)
dw_2.GetChild("defe_espmul", idwc_multipuerto)
dw_2.GetChild("defe_especi", idwc_especie)
dw_2.GetChild("defe_tecnic", idwc_cargotecnico)
dw_2.GetChild("defe_inspec", idwc_inspector)
dw_2.GetChild("tsad_randos", idwc_ingrediente)
dw_2.GetChild("tsag_numero", idwc_tratamiento)


dw_planta.SetTransObject(sqlca)
dw_plades.SetTransObject(sqlca)
dw_puerto.SetTransObject(sqlca)
dw_sitios.SetTransObject(sqlca)
idwc_multipuerto.SetTransObject(sqlca)
idwc_especie.SetTransObject(sqlca)
idwc_cargotecnico.SetTransObject(sqlca)
idwc_inspector.SetTransObject(sqlca)
idwc_ingrediente.SetTransObject(sqlca)
idwc_tratamiento.SetTransObject(sqlca)

dw_planta.Retrieve(1)
dw_plades.Retrieve(1)
dw_puerto.Retrieve(1)
dw_sitios.Retrieve(1)
idwc_especie.Retrieve()
idwc_multipuerto.Retrieve(-1)
idwc_cargotecnico.Retrieve(gi_codplanta)
idwc_inspector.Retrieve(gi_codplanta)
idwc_ingrediente.Retrieve('0')	
idwc_tratamiento.Retrieve()

dw_2.SetItem(1, "clie_codigo",gi_codexport)
istr_mant.argumento[1]= String(gi_codplanta)
istr_mant.argumento[3]= String(gi_codexport)
istr_mant.argumento[7]=''
istr_mant.argumento[27]='7'

pb_nuevo.PostEvent(Clicked!)

buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"
GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

dw_5.SetTransObject(sqlca)
dw_6.SetTransObject(sqlca)
dw_9.SetTransObject(sqlca)
dw_10.SetTransObject(sqlca)

dw_palletfrutahisto.SetTransObject(sqlca)
dw_palletencahisto.SetTransObject(sqlca)
dw_alpalletfruta.SetTransObject(sqlca)
dw_alpalletencab.SetTransObject(sqlca)

iuo_patente				=	CREATE	uo_patente
iuo_Especie				=	CREATE uo_Especie
iuo_clprv				=	Create uo_clienprove

// Para guardar fecha de despacho en la Palletfruta
ids_palletfruta_fecha				=	CREATE	DataStore
ids_palletfruta_fecha.DataObject	=	'dw_mues_palletfruta_pafrfecdes'
ids_palletfruta_fecha.SetTransObject(sqlca)

ids_CorrelMovim						=	CREATE	DataStore
ids_CorrelMovim.DataObject			=	'dw_mues_correlmoviemientos_despa'
ids_CorrelMovim.SetTransObject(sqlca)

dw_14.SetTransObject(sqlca)



end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, ll_folio, ll_null, ll_guia
integer	li_respuesta, li_cliente, li_planta, li_cont
String	ls_tratamiento

SetNUll(ll_null)

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[3]), &
										Integer(istr_mant.argumento[1]), &
										Long(istr_mant.argumento[2]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		pb_grabar.Enabled = True
		dw_2.Object.defe_numero.Protect	= 1
		dw_2.Object.plde_codigo.Protect 	= 1
		dw_2.Object.clie_codigo.Protect 	= 1

		dw_2.Object.defe_numero.Color	= Rgb(255,255,255)
		dw_2.Object.plde_codigo.Color		= Rgb(255,255,255)
		dw_2.Object.clie_codigo.Color 		= Rgb(255,255,255)
		
		dw_2.Object.defe_numero.BackGround.Color	= 553648127
		dw_2.Object.plde_codigo.BackGround.Color		= 553648127
		dw_2.Object.clie_codigo.BackGround.Color 		= 553648127
		
		IF ll_fila_e > 0 THEN
			
			ls_tratamiento = dw_2.Object.tsag_numero[1]
			dw_2.GetChild("tsad_randos", idwc_ingrediente)
			idwc_ingrediente.SetTransObject(sqlca)
			idwc_ingrediente.Retrieve(ls_tratamiento)	
			
			
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]),dw_2.Object.defe_plasag[1])
		END IF	
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1


end event

on w_maed_modificaplanilla.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_5=create dw_5
this.dw_6=create dw_6
this.dw_7=create dw_7
this.dw_8=create dw_8
this.dw_9=create dw_9
this.dw_10=create dw_10
this.dw_11=create dw_11
this.dw_12=create dw_12
this.dw_13=create dw_13
this.dw_14=create dw_14
this.dw_15=create dw_15
this.dw_alpalletencab=create dw_alpalletencab
this.dw_alpalletfruta=create dw_alpalletfruta
this.dw_palletencahisto=create dw_palletencahisto
this.dw_palletfrutahisto=create dw_palletfrutahisto
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.dw_5
this.Control[iCurrent+4]=this.dw_6
this.Control[iCurrent+5]=this.dw_7
this.Control[iCurrent+6]=this.dw_8
this.Control[iCurrent+7]=this.dw_9
this.Control[iCurrent+8]=this.dw_10
this.Control[iCurrent+9]=this.dw_11
this.Control[iCurrent+10]=this.dw_12
this.Control[iCurrent+11]=this.dw_13
this.Control[iCurrent+12]=this.dw_14
this.Control[iCurrent+13]=this.dw_15
this.Control[iCurrent+14]=this.dw_alpalletencab
this.Control[iCurrent+15]=this.dw_alpalletfruta
this.Control[iCurrent+16]=this.dw_palletencahisto
this.Control[iCurrent+17]=this.dw_palletfrutahisto
end on

on w_maed_modificaplanilla.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_5)
destroy(this.dw_6)
destroy(this.dw_7)
destroy(this.dw_8)
destroy(this.dw_9)
destroy(this.dw_10)
destroy(this.dw_11)
destroy(this.dw_12)
destroy(this.dw_13)
destroy(this.dw_14)
destroy(this.dw_15)
destroy(this.dw_alpalletencab)
destroy(this.dw_alpalletfruta)
destroy(this.dw_palletencahisto)
destroy(this.dw_palletfrutahisto)
end on

event ue_nuevo;HabilitaEncab(True)


ib_ok	= True

CHOOSE CASE wf_modifica()
	CASE -1
		ib_ok = False
	//CASE 0
//		CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
////			CASE 1
////				Message.DoubleParm = 0
////				This.TriggerEvent("ue_guardar")
////				IF message.DoubleParm = -1 THEN ib_ok = False
////			CASE 3
////				ib_ok	= False
////				RETURN
//		END CHOOSE
END CHOOSE

IF ib_ok = False THEN RETURN

dw_1.Reset()
dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()
dw_7.Reset()
dw_8.Reset()
dw_9.Reset()
dw_10.Reset()
dw_11.Reset()
dw_12.Reset()

dw_2.SetTabOrder("clie_codigo",1)
dw_2.SetTabOrder("plde_codigo",2)

dw_2.GetChild("tsad_randos", idwc_ingrediente)
idwc_ingrediente.SetTransObject(sqlca)
idwc_ingrediente.Retrieve('0')		
				
dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_codexport)
dw_2.SetItem(1, "plde_codigo", gi_codplanta)
dw_2.SetItem(1,"defe_horade", Now())

dw_2.SetRedraw(True)

dw_2.SetFocus()
end event

event ue_seleccion;call super::ue_seleccion;Str_busqueda			lstr_busq

lstr_busq.argum[1]	= 	istr_mant.argumento[3]
lstr_busq.argum[2]	= 	istr_mant.argumento[1]

OpenWithParm(w_busc_despafrigoen, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= lstr_busq.argum[2]
	istr_mant.argumento[2]	= lstr_busq.argum[5]
	istr_mant.argumento[3]  = lstr_busq.argum[1]
	ib_existe_folioD			=	True
	This.TriggerEvent("ue_recuperadatos")
		
	dw_2.GetChild("defe_espmul", idwc_multipuerto)
	idwc_multipuerto.SetTransObject(sqlca)
	idwc_multipuerto.Retrieve(dw_2.Object.defe_especi[1])
	
	IF il_cierra = 1 THEN
		Return
	END IF	
	istr_mant.argumento[8]	=	String(dw_2.Object.defe_cancaj[1])
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_filas, ll_fila_d,ll_fila_g, ll_nuevofolio, li_fillas, ll_destino, ll_planilla, ll_fila
Integer	li_control

ll_destino 	= 	dw_2.Object.defe_plades[1]
			  
ll_planilla	=	dw_2.Object.defe_plasag[1]

IF IsNull(ll_planilla) OR ll_planilla = 0 THEN
	MessageBox("Error de Consistencia", "Falta Número Planilla.", StopSign!, Ok!)
	dw_2.SetColumn("defe_plasag")
	Message.DoubleParm = -1
END IF	

IF IsNull(dw_2.Object.defe_espmul[1]) OR dw_2.Object.defe_espmul[1] = 0 THEN
	MessageBox("Error de Consistencia", "Falta Especie Multipuerto.", StopSign!, Ok!)
	dw_2.SetColumn("defe_espmul")
	Message.DoubleParm = -1
END IF	

IF IsNull(dw_2.Object.defe_tecnic[1]) OR dw_2.Object.defe_tecnic[1] = 0 THEN
	MessageBox("Error de Consistencia", "Falta Contraparte.", StopSign!, Ok!)
	dw_2.SetColumn("defe_tecnic")
	Message.DoubleParm = -1
END IF	
			  
SELECT isnull(plde_cajbul,0) 
INTO :li_control  
FROM dbo.plantadesp
WHERE plde_codigo = :ll_destino;

IF dw_2.GetNextModified(0, Primary!) > 0 THEN
	dw_2.SetItem(1, "defe_fecact", Today())
	dw_2.SetItem(1, "defe_horact", Now())
END IF

ids_palletfruta_fecha.Reset() 

FOR ll_fila = 1 TO dw_1.RowCount() 
	IF isnull(dw_1.Object.defe_plasag[ll_fila]) OR dw_1.Object.defe_plasag[ll_fila] = 0 THEN
		dw_1.Object.defe_plasag[ll_fila] = dw_2.Object.defe_plasag[1]
		dw_1.Object.plde_codigo[ll_fila] = dw_2.Object.plde_codigo[1]
	END IF	
		
	IF isnull(dw_1.Object.sell_numero[ll_fila]) OR dw_1.Object.sell_numero[ll_fila] = '' THEN
		MessageBox("Error de Consistencia", "Falta Número de sello.", StopSign!, Ok!)
		Message.DoubleParm = -1
		Return 
	END IF	
NEXT	
end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	
	IF dw_2.Object.defe_estado[1]	<>	1	THEN	pb_Eliminar.Enabled	=	True
	
	IF istr_mant.Solo_Consulta THEN
		dw_2.Enabled			=	False
		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled		=	False
		pb_ins_det.Enabled	=	False
		pb_eli_det.Enabled	=	False
	ELSE
		dw_2.Enabled			=	True
		pb_Eliminar.Enabled	=	True
		IF il_cierra <> 2 THEN
			pb_Grabar.Enabled		=	True
		END IF	
		pb_ins_det.Enabled	=	True
		pb_eli_det.Enabled	=	True
	END IF
ELSE
	IF istr_mant.Solo_Consulta THEN
		pb_ins_det.Enabled	=	False
	ELSE
		pb_ins_det.Enabled	=	True
	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

event ue_borrar;IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)

IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
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

event close;Boolean	Valida
Window	ventana
Integer	li_vta

ventana	= This.ParentWindow().GetFirstSheet()

IF IsValid(ventana) THEN
	li_vta++

	DO
		ventana	= this.ParentWindow().GetNextSheet(ventana)
		valida	= IsValid(ventana)
		IF valida THEN li_vta++
	LOOP WHILE valida
END IF
end event

event ue_nuevo_detalle;call super::ue_nuevo_detalle;il_fila = dw_1.InsertRow(0)

IF il_fila > 0 THEN
	pb_eliminar.Enabled	= True
	pb_grabar.Enabled	= True
END IF

dw_1.ScrollToRow(il_fila)
dw_1.SetRow(il_fila)
dw_1.SetFocus()
dw_1.SetColumn(1)
end event

event ue_borra_detalle;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

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

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

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
end event

event ue_validaborrar;//
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_modificaplanilla
integer x = 27
integer y = 1460
integer width = 2327
integer height = 684
integer taborder = 100
string title = "Detalle Ubicación Sellos"
string dataobject = "dw_mues_ubicacionsellos"
boolean livescroll = false
end type

event dw_1::retrieveend;If RowCount() > 0 Then	pb_ins_det.Enabled = TRUE
end event

event dw_1::clicked;IF Row > 0 THEN
	il_fila = Row
	This.SelectRow(0,False)
	This.SetRow(il_fila)
	//This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event dw_1::rowfocuschanged;ib_datos_ok = True

IF rowcount() < 1 OR getrow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila = getrow()
	This.SelectRow(0,False)
//	This.SelectRow(il_fila,True)
END IF

RETURN 0
end event

event dw_1::getfocus;//IF il_fila > 0 THEN This.SelectRow(il_fila, True)

RETURN 0
end event

event dw_1::itemchanged;Long		ll_null

SetNull(ll_null)

CHOOSE CASE dwo.Name
	
	CASE "sell_numero"
		IF Duplicado(data) THEN
			This.SetItem(il_fila, "sell_numero", String(ll_null))
			RETURN 1
		END IF
		
	CASE "sell_codigo"	
		
		IF existeubicacionsello(data) THEN
			This.SetItem(il_fila, "sell_codigo", String(ll_null))
			RETURN 1
		END IF	

END CHOOSE

end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_modificaplanilla
integer x = 82
integer y = 28
integer width = 1673
integer height = 1360
string dataobject = "dw_mant_modificaplanilla"
boolean livescroll = true
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null, ll_Planilla, ll_tpcont, ll_orcont, ll_guides, ll_sello, ll_despacho
String	ls_columna, ls_null, ls_embq_codigo, ls_TipoPla, &
			ls_glosa,ls_glosag,ls_ubisel,ls_numsel,ls_nrcont,ls_patent,ls_pataco, ls_chofer, ls_rutchofer
Date		ld_nula
Integer	li_Cliente, li_Planta, li_existe, li_espmul, li_sicodigo, li_trancodigo, li_ticacodigo

SetNull(ll_null)
SetNull(ls_null)
SetNull(ld_nula)
ls_columna = dwo.Name

CHOOSE CASE ls_columna
	CASE "clie_codigo"
		istr_mant.argumento[3]	= data
		IF F_ValidaCliente(Integer(data)) = False THEN
			dw_2.SetItem(il_fila, "clie_codigo", gi_codexport)
			RETURN 1
		ELSE
			dw_2.GetChild("plde_codigo", dw_planta)
			dw_planta.SetTransObject(sqlca)
			dw_planta.Retrieve(1)
			
			dw_2.SetItem(1, "embq_nomnav", ls_null)
			dw_2.SetItem(1, "embq_codigo", ls_null)
			
			dw_2.GetChild("defe_plades", dw_plades)
			dw_plades.SetTransObject(sqlca)
			dw_plades.Retrieve(1)
		END IF
					
	CASE "plde_codigo"
		ExisteFolio(ls_columna, data)
		dw_2.GetChild("defe_plades", dw_plades)
		dw_plades.SetTransObject(sqlca)
		dw_plades.Retrieve(1)
		
		dw_2.GetChild("defe_tecnic", idwc_cargotecnico)
		idwc_cargotecnico.SetTransObject(sqlca)
		idwc_cargotecnico.Retrieve(Integer(data))		
		
	CASE "defe_numero"
		//ExisteFolio(ls_columna, data)
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(1, ls_columna, Integer(ls_null))
			RETURN 1
		END IF
		
		IF il_cierra = 1 THEN
			Pb_salir.TriggerEvent(Clicked!)
			Return
		END IF
		
	CASE "defe_nturno"		
		This.SetItem(1, "defe_plasag", Integer(ls_null))
		
	CASE "defe_plasag"
		IF existe_plasag(Integer(istr_mant.argumento[1]),Long(data)) THEN
			This.SetItem(1, ls_columna, Integer(ls_null))
			Return 1
		END IF	
	
		IF isnull(dw_2.Object.defe_nturno[1]) THEN
			MessageBox("Atención","Falta Ingreso de Tipo Planilla.")
			This.SetItem(1, ls_columna, Integer(ls_null))
			Return 1
		END IF	
	
		IF planillaanulada(Integer(istr_mant.argumento[1]),Long(data),dw_2.Object.defe_nturno[1]) THEN
			This.SetItem(1, ls_columna, Integer(ls_null))
			Return 1
		END IF
		
		IF Not Isnull(data) THEN
			dw_2.SetTabOrder("defe_espmul",180)
			dw_2.Modify("defe_espmul.BackGround.Color = " + String(rgb(255,255,255)))
			
			ll_Planilla	=	Long(data)
			ls_TipoPla	=	dw_2.Object.defe_nturno[1]
			li_Cliente	=	Integer(istr_mant.argumento[3]) 
			li_Planta	=	Integer(istr_mant.argumento[1])
			
			SELECT Max(defe_guides)
			INTO :ll_guides
			FROM dbo.despafrigoen 
			WHERE clie_codigo	=	:li_Cliente
			AND   plde_codigo	=	:li_Planta
			AND   defe_nturno	=	:ls_TipoPla
			AND   defe_plasag	=	:ll_Planilla;
			
			SELECT count(*),defe_glosas,defe_glosag,defe_ubisel,defe_numsel,defe_nrcont,defe_tpcont,defe_orcont,defe_patent,defe_pataco,
				sire_codigo, defe_espmul, tran_codigo, tica_codigo, defe_chofer, defe_chfrut
			INTO :li_existe, :ls_glosa, :ls_glosag,: ls_ubisel, :ls_numsel, :ls_nrcont, :ll_tpcont, :ll_orcont, :ls_patent, :ls_pataco,
				:li_sicodigo, :li_espmul, :li_trancodigo, :li_ticacodigo, :ls_chofer, :ls_rutchofer
			FROM dbo.despafrigoen 
			WHERE clie_codigo	=	:li_Cliente
			AND   plde_codigo	=	:li_Planta
			AND   defe_nturno	=	:ls_TipoPla
			AND   defe_plasag	=	:ll_Planilla
			AND   defe_guides =  :ll_guides
			group by defe_glosas,defe_glosag,defe_ubisel,defe_numsel,defe_nrcont,defe_tpcont,defe_orcont,defe_patent,defe_pataco,
				sire_codigo, defe_espmul, tran_codigo, tica_codigo, defe_chofer, defe_chfrut;
			
			IF li_existe > 0 THEN
				
				This.Object.sire_codigo[1] = li_sicodigo
				This.Object.defe_glosas[1] = ls_glosa
				This.Object.defe_ubisel[1] = ls_ubisel
				This.Object.defe_numsel[1] = ls_numsel
				This.Object.defe_nrcont[1] = ls_nrcont
				This.Object.defe_tpcont[1] = ll_tpcont
				This.Object.defe_orcont[1] = ll_orcont
				This.Object.defe_patent[1] = ls_patent
				This.Object.defe_glosag[1] = ls_glosag
				This.Object.defe_pataco[1] = ls_pataco
				This.Object.defe_espmul[1] = li_espmul
				This.Object.tran_codigo[1] = li_trancodigo
				This.Object.tica_codigo[1] = li_ticacodigo
				This.Object.defe_chofer[1] = ls_chofer
				This.Object.defe_chfrut[1] = ls_rutchofer
				
			END IF	
		ELSE
			This.SetItem(1, 'defe_nturno', ls_null)
			This.SetItem(1, 'sire_codigo', Integer(ls_null))
			This.SetItem(1, 'defe_glosag', ls_null)
			This.SetItem(1, 'defe_espmul', Integer(ls_null))
			dw_2.SetTabOrder("defe_espmul",0)
			dw_2.Modify("defe_espmul.BackGround.Color = " + String(RGB(166,180,210)))
		END IF
	
	CASE "defe_especi"
		IF iuo_Especie.Existe(Integer(data),True,SQLCA) THEN
			istr_mant.argumento[10] = String(Data)	
			IF idwc_multipuerto.Retrieve(Integer(data)) = 0 THEN
				idwc_multipuerto.InsertRow(0)
			END IF
		ELSE
			This.SetItem(row,'defe_especi',Integer(ls_null))
			RETURN 1
		END IF
	
	CASE "trat_codigo"
		IF dw_2.Object.defe_tiposa[1] = 7 OR dw_2.Object.defe_tiposa[1] = 8 OR dw_2.Object.defe_tiposa[1] = 9 THEN
			
			
			IF Integer(il_desreferencia) <> il_destino THEN
				MessageBox("Atención","Destino Seleccionado no Corresponde al Embarque.")
			END IF	
	
		END IF
	
	CASE "tsag_numero"
		IF existe_tratamiento(data) THEN
			This.SetItem(1, 'tsag_numero', ls_null)
			Return 1
		END IF	
		
	CASE "inac_codigo"
		IF existe_ingredienteactivo(data) THEN
			This.SetItem(1, 'inac_codigo', ls_null)
			Return 1
		END IF	
		
	CASE "temp_codigo"
		IF existe_temperatura(Long(data)) THEN
			This.SetItem(1, 'temp_codigo', Long(ls_null))
			Return 1
		END IF	
		
	CASE "dura_codigo"
		IF existe_duracion(data) THEN
			This.SetItem(1, 'dura_codigo', ls_null)
			Return 1
		END IF	
		
	CASE "cons_codigo"
		IF existe_concentracion(data) THEN
			This.SetItem(1, 'cons_codigo', ls_null)
			Return 1
		END IF		
	
		
					
END CHOOSE


end event

event dw_2::doubleclicked;//
end event

event dw_2::clicked;call super::clicked;CHOOSE CASE dwo.name
		
	CASE "buscacliente"
		buscacliente()
   		//HabilitaIngreso('clie_codigo')
			
	
END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_modificaplanilla
integer x = 2263
integer y = 364
end type

event pb_nuevo::clicked;call super::clicked;pb_grabar.Enabled = False

dw_2.SetTabOrder("defe_numero",30)
dw_2.SetTabOrder("plde_codigo",20)
dw_2.SetTabOrder("clie_codigo",10)
dw_2.SetTabOrder("defe_espmul",90)

dw_2.Modify("defe_numero.BackGround.Color = " + String(rgb(255,255,255)))
dw_2.Modify("plde_codigo.BackGround.Color = " + String(rgb(255,255,255)))
dw_2.Modify("clie_codigo.BackGround.Color = " + String(rgb(255,255,255)))
dw_2.Modify("defe_espmul.BackGround.Color = " + String(rgb(255,255,255)))
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_modificaplanilla
boolean visible = false
integer x = 2080
integer y = 1224
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_modificaplanilla
integer x = 2258
integer y = 680
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_modificaplanilla
boolean visible = false
integer x = 2350
integer y = 1348
boolean default = true
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_modificaplanilla
integer x = 2263
integer y = 980
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_modificaplanilla
integer x = 2496
integer y = 1680
boolean enabled = true
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_modificaplanilla
integer x = 2496
integer y = 1860
boolean enabled = true
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_modificaplanilla
integer x = 2263
integer y = 184
end type

event pb_buscar::clicked;call super::clicked;IF il_cierra = 1 THEN
	Pb_salir.TriggerEvent(Clicked!)
	Return
END IF	
dw_3.Reset()
dw_4.Reset()
dw_5.Reset()
dw_6.Reset()
dw_7.Reset()
dw_8.Reset()
dw_9.Reset()
dw_10.Reset()
dw_11.Reset()
dw_12.Reset()
end event

type dw_3 from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 46
integer y = 2532
integer width = 178
integer height = 116
integer taborder = 100
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_recfruprocee_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 197
integer y = 2728
integer width = 3552
integer height = 624
integer taborder = 110
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_recfruproced_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 2825
integer y = 2636
integer width = 663
integer height = 248
integer taborder = 120
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_palletencab_historia"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_6 from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 480
integer y = 2852
integer width = 2094
integer height = 664
integer taborder = 130
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_palletfruta_despa"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_7 from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 146
integer y = 2732
integer width = 178
integer height = 116
integer taborder = 120
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_palletencab_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_8 from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 59
integer y = 2652
integer width = 2926
integer height = 816
integer taborder = 130
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_palletfruta_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_9 from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 1253
integer y = 2620
integer width = 178
integer height = 116
integer taborder = 120
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspec"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_10 from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 1454
integer y = 2620
integer width = 178
integer height = 116
integer taborder = 130
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspecdet"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_11 from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 1655
integer y = 2620
integer width = 178
integer height = 116
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspec_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_12 from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 1856
integer y = 2620
integer width = 178
integer height = 116
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_recupera_palletinspecdet_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_13 from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 2085
integer y = 2624
integer width = 165
integer height = 112
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_gene_archivo_saam_plano"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_14 from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 2683
integer y = 2568
integer width = 306
integer height = 408
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_cajasprod_caja"
boolean resizable = true
boolean livescroll = true
end type

type dw_15 from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 2427
integer y = 2572
integer width = 233
integer height = 400
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_cajasprod_trans"
boolean livescroll = true
end type

type dw_alpalletencab from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 119
integer y = 2632
integer width = 686
integer height = 400
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_alpalletencab"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_alpalletfruta from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 2665
integer y = 2648
integer width = 686
integer height = 400
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_alpalletfruta"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_palletencahisto from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 1431
integer y = 2812
integer width = 686
integer height = 400
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_palletencabhisto_inpe"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_palletfrutahisto from datawindow within w_maed_modificaplanilla
boolean visible = false
integer x = 2711
integer y = 2984
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_palletfrutahisto_inpe"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

