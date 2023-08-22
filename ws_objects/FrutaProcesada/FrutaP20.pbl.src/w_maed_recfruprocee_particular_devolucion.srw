$PBExportHeader$w_maed_recfruprocee_particular_devolucion.srw
forward
global type w_maed_recfruprocee_particular_devolucion from w_mant_encab_deta_csd
end type
end forward

global type w_maed_recfruprocee_particular_devolucion from w_mant_encab_deta_csd
integer width = 3982
integer height = 2044
string title = "DEVOLUCION DE PUERTO"
string menuname = ""
event ue_imprimir ( )
event ue_validaregistro ( )
event ue_despuesguardar ( )
event ue_cargarchivoplano ( )
event ue_imprimir2 ( )
event ue_traedatos ( )
end type
global w_maed_recfruprocee_particular_devolucion w_maed_recfruprocee_particular_devolucion

type variables
w_mant_deta_recfruprocee iw_mantencion

DataWindowChild	dw_puerto, dw_planta, dw_ptaori, dw_fruta, idwc_patente
Integer 	ii_recepcion, ii_cliente, ii_planta, ii_estado, ii_controlaaceso,ii_borra=0, ii_controlpallet, ii_TipoEnt, ii_Packing
Boolean	ib_existe_folio, ib_primera_entrada, ib_conectado
Long     il_pallet, il_folio, il_numero, il_NroCaja
Date     id_mespro 
String   is_Archivo, is_mensaje



uo_pallet_trans			iuo_pallet
uo_especie				iuo_especie
uo_variedades			iuo_variedades
uo_embalajesprod		iuo_embalajesprod
uo_etiquetas			iuo_etiquetas
uo_tipofrio				iuo_tipofrio	
uo_status				iuo_status	
uo_tipopallet			iuo_tipopallet	
uo_condicion			iuo_condicion	
uo_codigopallet			iuo_codigopallet	
uo_destinos				iuo_destinos
uo_productores			iuo_productores	
uo_calibre				iuo_calibre
uo_categoria        	 	iuo_categoria
uo_tratamiento       	iuo_tratamiento
uo_frutarecepcion		iuo_frutarecepcion
uo_patente				iuo_patente

Transaction	sqlconec



end variables

forward prototypes
public function boolean noexisteproductor (string as_columna, string as_valor)
public subroutine cuentatarjas ()
public function boolean noexistecliente (integer ai_codigo)
public subroutine buscaproductor ()
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean noexisteplanta (string columna)
public function boolean existefolio (string as_columna, string as_valor)
public subroutine eliminapallet (long pallet)
public function integer cajaspallet (string as_embalaje)
public subroutine graba_distribucproducc ()
public function long buscafoliorecfruprocee (integer ai_planta)
public function boolean existespro_palletencab (integer ai_cliente, integer ai_planta, long al_pallet)
public function long buscanuevofolio (integer planta)
public subroutine habilitaingreso (string as_columna)
public function boolean coneccionbase ()
public subroutine buscaembarque ()
public function boolean noexisteembarque (string as_columna, string as_valor)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)
Long		fila
Date		ld_desde, ld_hasta
String   ls_recepcion, ls_descri

str_info	lstr_info

lstr_info.titulo	= "RECEPCION DE PALLETS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_pallettransitorios"
vinf.dw_1.SetTransObject(sqlca)

istr_mant.argumento[30] = String(dw_2.Object.rfpe_numero[1])

ls_recepcion = "Recepcion " +String(dw_2.Object.rfpe_numero[1])
ls_recepcion = ls_recepcion + "                       Fecha "+String(dw_2.Object.rfpe_fecrec[1])
ls_recepcion = ls_recepcion + "                       Guia "+String(dw_2.Object.rfpe_nrores[1])
ls_recepcion = ls_recepcion + "                       En Planta "+String(dw_2.Object.plde_codigo[1])

fila = vinf.dw_1.Retrieve(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_2.Object.rfpe_numero[1])

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
  	vinf.dw_1.Modify("guia.text = '" + ls_recepcion + "'")
	vinf.dw_1.Modify("fruta.text = '" + ls_descri + "'")  
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)
end event

event ue_traedatos();Integer	li_cliente, li_planta
Long	ll_numero

ll_numero  = Long(istr_mant.argumento[37])
li_cliente = Integer(istr_mant.argumento[3])
li_planta= Integer(istr_mant.argumento[1])

dw_2.retrieve(li_planta,ll_numero,li_cliente)

dw_1.retrieve(li_planta,ll_numero,li_cliente)
end event

public function boolean noexisteproductor (string as_columna, string as_valor);String	ls_nombre
Integer	li_cliente
Long		ll_product

li_cliente	=	dw_2.Object.clie_codigo[1]
ll_product	=	dw_2.Object.prod_codigo[1]

CHOOSE CASE as_Columna
	CASE "clie_codigo"
		li_cliente	=	Integer(as_valor)

	CASE "prod_codigo"
		ll_product	=	Long(as_valor)
		
END CHOOSE

IF IsNull(li_cliente) = False AND li_cliente > 0 AND &
	IsNull(ll_product) = False AND ll_product > 0 THEN
	SELECT	prod_nombre 
	INTO :ls_nombre
	FROM	dbo.productores
	WHERE	prod_codigo	=	:ll_product ;
				
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Productores")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención ", "Código de Productor no ha sido ingresado." + &
						"~n~nIngrese o Seleccione otro.", &
						Exclamation!, OK!) 
		RETURN True
	ELSE
		dw_1.SetItem(1, "prod_codigo", ls_nombre)
		RETURN False
	END IF
ELSE
	RETURN False
END IF
end function

public subroutine cuentatarjas ();Long 		I,ll_tra=0,ll_def=0
Integer	li_Tarjas, li_Tardef

FOR I=1 TO dw_1.Rowcount()
	IF dw_1.Object.paen_tipopa[I]=1 THEN
		ll_def ++
	ELSE
		ll_tra ++
	END IF
NEXT

li_Tarjas = dw_2.Object.rfpe_tarjas[1]
li_Tardef = dw_2.Object.rfpe_tardef[1]

istr_mant.argumento[10]	= String( li_Tarjas + li_Tardef)
istr_mant.argumento[11]	= String(dw_2.Object.rfpe_tardef[1])
istr_mant.argumento[12]	= String(dw_2.Object.rfpe_tarjas[1])
		
RETURN
end subroutine

public function boolean noexistecliente (integer ai_codigo);String	ls_nombre

SELECT	clie_nombre
INTO	:ls_nombre  
FROM	dbo.clientesprod  
WHERE	clie_codigo =	:ai_codigo;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Clientes Producción")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "Cliente no Existe. Ingrese otro.")
	RETURN True
END IF

IF F_ValidaCliente(ai_codigo) THEN
	dw_2.GetChild("plde_codigo", dw_planta)
	dw_2.GetChild("rfpe_ptaori", dw_ptaori)
	dw_planta.SetTransObject(sqlca)
	dw_ptaori.SetTransObject(sqlca)
	istr_mant.Argumento[3]	=	String(ai_codigo)
	dw_planta.Retrieve(1)
	dw_ptaori.Retrieve()
	RETURN False
ELSE
	RETURN True
END IF

end function

public subroutine buscaproductor ();Str_busqueda	lstr_busq

dw_2.Modify("buscaproductor.border = 5")

lstr_busq.Argum[1]	=	istr_mant.Argumento[3]

OpenWithParm(w_busc_productores, istr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[3] <> "" THEN
	dw_2.setItem(1, "prod_codigo", lstr_busq.argum[3])
	dw_2.setItem(1, "prod_nombre", lstr_busq.argum[4])
ELSE
	dw_2.SetColumn("prod_codigo")
	dw_2.SetFocus()
END IF

dw_2.Modify("buscaproductor.border = 6")
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect	=	0
	dw_2.Object.rfpe_numero.Protect	=	0
	dw_2.Object.plde_codigo.Protect	=	0
	dw_2.Object.rfpe_fecrec.Protect	=	0

	dw_2.Object.clie_codigo.Color 		= 0
	dw_2.Object.rfpe_numero.Color 	= 0
	dw_2.Object.plde_codigo.Color 	= 0
	dw_2.Object.rfpe_fecrec.Color 		= 0
	
	dw_2.Object.clie_codigo.BackGround.Color 		= Rgb(255,255,255)
	dw_2.Object.rfpe_numero.BackGround.Color 	= Rgb(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color 	= Rgb(255,255,255)
	dw_2.Object.rfpe_fecrec.BackGround.Color 		= Rgb(255,255,255)
	
	dw_2.SetColumn("rfpe_numero")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clie_codigo.Protect	=	1
	dw_2.Object.rfpe_numero.Protect	=	1
	dw_2.Object.plde_codigo.Protect	=	1
	
	dw_2.Object.clie_codigo.Color 		= Rgb(255,255,255)
	dw_2.Object.rfpe_numero.Color	= Rgb(255,255,255)
	dw_2.Object.plde_codigo.Color 	= Rgb(255,255,255)
	
	dw_2.Object.clie_codigo.BackGround.Color 		= 553648127
	dw_2.Object.rfpe_numero.BackGround.Color	= 553648127
	dw_2.Object.plde_codigo.BackGround.Color 	= 553648127

END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno
Long numero
Integer li_planta, li_movto

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
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
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
END IF

Numero = Long(istr_mant.argumento[2])
li_planta = Integer(istr_mant.argumento[1])

li_movto = 1

/*actualiza numero actual en correlativos
update dbo.CORRELMOVIMIENTOS set
COMO_ACTUAl = :numero
where plde_codigo = :li_planta
And	como_tipomv = :li_movto;
 */
 
sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean noexisteplanta (string columna);Integer	li_cliente, li_planta, li_tipo

li_cliente	=	Integer(istr_mant.argumento[3])
li_planta	=	Integer(columna)
li_tipo		=	Integer(istr_mant.argumento[13])

SELECT	plde_codigo
INTO	:li_planta 
FROM	dbo.plantadesp  
WHERE plde_codigo	=	:li_planta
AND	plde_tipopl	=	:li_tipo ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla PLANTADESP")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Codigo Planta/Packing no Existe. Ingrese otro")
	RETURN True
END IF

istr_mant.argumento[15]	=	String(li_planta)

RETURN False
end function

public function boolean existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe,li_cliente, li_tipoen, li_cont
Long		ll_nfolio

li_planta	=	dw_2.Object.plde_codigo[1]
ll_nfolio 	=	dw_2.Object.rfpe_numero[1]
li_cliente	=  dw_2.Object.clie_codigo[1]

CHOOSE CASE as_columna
	CASE "plde_codigo"
		li_planta	=	Integer(as_valor)
		
	CASE "rfpe_numero"
		ll_nfolio 	=	Long(as_valor)
		
	CASE "clie_codigo"
		li_cliente 	=	Integer(as_valor)	
		
END CHOOSE

SELECT  	rfpe_tipoen
INTO	:li_tipoen
FROM	dbo.recfruprocee
WHERE	plde_codigo	=	:li_planta
AND	rfpe_numero	=	:ll_nfolio
AND	rfpe_estado = 2;
			
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla RecFruProcee")
	RETURN False
ELSEIF sqlca.SQLCode = 0 THEN
	MessageBox("Atención","Recepción ya es Definitiva, Ingrese Otra Recepción.")
	RETURN True
ELSE
	IF IsNull(ll_nfolio) THEN
		istr_mant.argumento[1]	= String(li_planta)
		istr_mant.argumento[2]	= String(ll_nfolio)
		istr_mant.argumento[3]	= String(li_cliente)
		ib_existe_folio	=	False
		RETURN False
	ELSE
		SELECT  	count()
		INTO	:li_cont
		FROM	dbo.recfruprocee
		WHERE	plde_codigo	=	:li_planta
		AND	rfpe_numero	=	:ll_nfolio;
		
		IF li_cont = 0 THEN
			MessageBox("Atención","Número de Documento No ha sido generado. Ingrese Otro.")
			ib_existe_folio	=	False
		ELSE
			istr_mant.argumento[1]	= String(li_planta)
			istr_mant.argumento[2]	= String(ll_nfolio)
			istr_mant.argumento[3]	= String(li_cliente) 
			istr_mant.argumento[20]= String(li_tipoen)
			istr_mant.argumento[4]	= String(dw_2.Object.rfpe_tarjas[1])
			dw_2.SetItem(1, "clie_codigo",li_cliente)
			dw_2.SetItem(1, "plde_codigo",li_planta)
			This.TriggerEvent("ue_recuperadatos")
			IF li_tipoen = 1 THEN
				dw_ptaori.Setfilter("plde_tipopl=2")
				dw_ptaori.Filter()
			ELSE
				dw_ptaori.Setfilter("plde_tipopl=1")
				dw_ptaori.Filter()
			END IF
			ib_existe_folio	=	True
		END IF	
		RETURN True
	END IF
END IF

end function

public subroutine eliminapallet (long pallet);Integer 	li_cliente,li_planta
Long		ll_palet,ll_palet1,ll_palet2,ll_palet3,ll_palet4,ll_palet5,ll_palet6

li_planta	=	Integer(istr_mant.argumento[1])
li_cliente	=	Integer(istr_mant.argumento[3])

SELECT count(*) INTO :ll_palet1
FROM dbo.despafrigode
WHERE clie_codigo	=	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

SELECT count(*) INTO :ll_palet2
FROM dbo.repalletdeta
WHERE clie_codigo = 	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

SELECT count(*) INTO :ll_palet3
FROM dbo.inspecpaldet
WHERE clie_codigo = 	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

SELECT count(*) INTO :ll_palet4
FROM dbo.fumigadet
WHERE clie_codigo = 	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

SELECT count(*) INTO	:ll_palet5
FROM dbo.reetidet
WHERE clie_codigo = 	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

SELECT count(*) INTO :ll_palet6
FROM dbo.alpalletfruta
WHERE clie_codigo = 	:li_cliente 
AND   plde_codigo = 	:li_planta
AND   paen_numero = 	:pallet;

IF IsNull(ll_palet1) THEN	ll_palet1	=	0
IF IsNull(ll_palet2) THEN	ll_palet2 	= 	0
IF IsNull(ll_palet3) THEN	ll_palet3 	= 	0
IF IsNull(ll_palet4) THEN	ll_palet4 	= 	0
IF IsNull(ll_palet5) THEN	ll_palet5 	= 	0
IF IsNull(ll_palet6) THEN	ll_palet6 	= 	0

ll_palet		=	ll_palet1+ll_palet2+ll_palet3+ll_palet4+ll_palet5+ll_palet6

IF ll_palet	<>	0 THEN
	MessageBox( "Error", "Pallet con Relación en Otras Tablas, Solo Elimina en Recepción.")	
	
END IF

RETURN
end subroutine

public function integer cajaspallet (string as_embalaje);Integer	li_CajasPallet, li_Cliente

li_Cliente = dw_2.Object.clie_codigo[1]

SELECT	emba_cajpal
INTO	:li_CajasPallet
FROM	dbo.embalajesprod
WHERE	emba_codigo	=	:as_Embalaje 
AND	clie_codigo =  :li_Cliente
USING sqlca;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla de Embalajesprod")
	
	li_CajasPallet	=	0
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Embalaje " + as_Embalaje + " no ha sido Creado en " + &
					"tabla respectiva.~r~rAvise a Encargado de Sistema.")
					
	li_CajasPallet	=	0
ELSEIF li_CajasPallet = 0 THEN
	MessageBox("Atención", "Embalaje " + as_Embalaje + " no tiene cantidad de Cajas " + &
					"por Pallet.~r~rAvise a Encargado de Sistema.")
END IF

RETURN li_CajasPallet
end function

public subroutine graba_distribucproducc ();
end subroutine

public function long buscafoliorecfruprocee (integer ai_planta);Integer	li_planta, li_cliente
Long		ll_numero
Boolean	lb_nulo

//li_cliente	=	cliente	
li_planta	=	ai_planta

SELECT Max(rfpe_numero)
INTO :ll_numero
FROM dbo.recfruprocee
WHERE plde_codigo = :li_planta
USING sqlca ;

SELECT como_actual 
INTO :ll_numero
FROM dbo.correlmovimientos
WHERE plde_codigo = :li_planta
AND	como_tipomv = 11;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla recfruprocee")
ELSEIF sqlca.SQLCode = 0 THEN
	
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

public function boolean existespro_palletencab (integer ai_cliente, integer ai_planta, long al_pallet);Boolean 	lb_Retorno
Long		ll_Cantid

lb_Retorno	= False

SELECT Count(*)
INTO	:ll_Cantid
FROM	dbo.spro_palletencab
WHERE	clie_codigo	= 	:ai_Cliente
AND	paen_numero = 	:al_Pallet
AND	plde_codigo	=	:ai_Planta;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Spro_PalletEncab")
	lb_retorno = True
ELSEIF ll_Cantid > 0 THEN	
	//MessageBox("Atención","No se puede Eliminar el Pallet es Ingreso Caja a Caja. Ingrese Otro.", Exclamation!, Ok!)
	lb_retorno = True		
END IF
		
RETURN lb_Retorno
end function

public function long buscanuevofolio (integer planta);//Integer	li_planta, li_cliente
//Long		ll_numero
//Boolean	lb_nulo
//
//li_planta	=	planta
//
//SELECT Max(rfpe_numero)
//  INTO :ll_numero
//  FROM dbo.RECFRUPROCEE
// WHERE plde_codigo = :li_planta;
//
//IF sqlca.SQLCode = -1 THEN
//	F_errorbasedatos(sqlca,"Lectura tabla RecFruProcee")
//ELSEIF sqlca.SQLCode = 0 THEN
//	
//		 lb_nulo = IsNull(ll_numero)	
//	
//	    IF lb_nulo THEN
//			 ll_numero=li_planta*10000
//		 ELSE
//			 ll_numero++
//		 END IF
//ELSE
//	ll_numero=li_planta*10000
//END IF
//
//RETURN ll_numero

Integer	li_planta, li_tipoins, li_movto
Long		ll_numero,ll_numero2, ll_fin, ll_actual

li_planta	=	planta

li_movto = 1

SELECT Max(rfpe_numero)
  INTO :ll_numero
  FROM dbo.RECFRUPROCEE
 WHERE plde_codigo = :li_planta;
 
Select como_inicia, como_actual, como_termin
Into	:ll_numero2, :ll_actual, :ll_fin
from dbo.CORRELMOVIMIENTOS 
Where plde_codigo = :li_planta
and	COMO_TIPOMV = :li_movto;

IF ll_actual >= ll_fin THEN
	Return 0
END IF	

ll_fin = ll_fin - 3

IF ll_actual >= ll_fin THEN 
	MessageBox("Advertencia","Quedan menos de 3 Correlativos, Proceda por Mantención 'Correlativos'")
END IF	

IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla CORRELMOVIMIENTOS")
END IF

IF Isnull(ll_numero) OR String(ll_numero) = '' or ll_numero < ll_numero2 THEN
	ll_numero = ll_numero2
END IF	


IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla RecFruProcee")
ELSEIF sqlca.SQLCode = 0 THEN
	ll_numero++
END IF

RETURN ll_numero

end function

public subroutine habilitaingreso (string as_columna);Date		ld_Fecha
Boolean	lb_Estado = True

dw_2.AcceptText()

IF Isnull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1]	=	0	OR &
	IsNull(dw_2.Object.rfpe_nrores[1]) OR dw_2.Object.rfpe_nrores[1] = 0 OR &
	IsNull(dw_2.Object.rfpe_fecrec[1]) OR dw_2.Object.rfpe_fecrec[1] = ld_Fecha OR &
	IsNull(dw_2.Object.tica_codigo[1]) OR dw_2.Object.tica_codigo[1] = 0 OR &
	IsNull(dw_2.Object.puer_codigo[1]) OR dw_2.Object.puer_codigo[1] = 0 OR &
	IsNull(dw_2.Object.embq_codigo[1]) OR dw_2.Object.embq_codigo[1] = '' THEN
	lb_Estado	=	False
END IF		

pb_ins_det.Enabled	=	lb_Estado
//pb_eli_det.Enabled	=	lb_Estado
//pb_imprimir.Enabled	=	lb_Estado



end subroutine

public function boolean coneccionbase ();String	ls_Usuario, ls_Password, ls_nomodb, ls_nomser, ls_nombas, ls_nodbms, ls_planta


DISCONNECT USING sqlconec;

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

  SELECT cone_nomodb,cone_nomser,cone_nombas,
         cone_nodbms,cone_nomusu,cone_passwo  
    INTO :ls_nomodb,:ls_nomser,:ls_nombas,
	      :ls_nodbms,:ls_Usuario,:ls_Password
    FROM dbo.prodconectividad   
   WHERE cone_codigo = 90;

sqlconec.ServerName	=	ls_nomser
sqlconec.DataBase	   =	ls_nombas
sqlconec.Dbms			= 	ls_nodbms
sqlconec.DbParm		=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'

CONNECT USING sqlconec;

IF sqlconec.SQLCode = 0 THEN
	ib_Conectado	=	True
ELSE
	ib_Conectado	=	False
END IF

RETURN ib_Conectado

end function

public subroutine buscaembarque ();Str_busqueda	lstr_busq

istr_busq.Argum[1]	=	String(dw_2.Object.clie_codigo[1])

OpenWithParm(w_busc_embarques, istr_busq)

lstr_busq	= Message.PowerObjectParm

If UpperBound(lstr_busq.Argum) > 5 Then
	If lstr_busq.Argum[1] <> "" Then
		dw_2.setItem(1, "embq_codigo", lstr_busq.argum[1])
		dw_2.setItem(1, "embq_nomnav", lstr_busq.argum[2])
		dw_2.setItem(1, "puer_codigo", Integer(lstr_busq.argum[6]))	
		istr_mant.argumento[24]	=	lstr_busq.argum[1]
	Else
		dw_2.SetColumn("embq_codigo")
		dw_2.SetFocus()
	End If
End If


end subroutine

public function boolean noexisteembarque (string as_columna, string as_valor);String	ls_nombre, ls_Codigo
Integer	li_Cliente, li_Puerto
Date		ld_fzarpe

ls_Codigo	=	dw_2.Object.embq_codigo[1]
li_Cliente	=	dw_2.Object.clie_codigo[1]

CHOOSE CASE as_Columna
	CASE "embq_codigo"
		ls_codigo	=	as_valor

END CHOOSE

IF IsNull(ls_codigo) = False AND ls_codigo <> "" THEN
	SELECT	embq_nomnav, embq_fzarpe, embq_ptoori 
	INTO :ls_nombre, :ld_fzarpe, :li_Puerto
	FROM	dbo.embarqueprod
	WHERE	embq_codigo	=	:ls_Codigo
	AND   clie_codigo =	:li_Cliente ;
				
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Embarqueprod")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención ", "Código de Embarque no ha sido ingresado." + &
						"~n~nIngrese o Seleccione otro.", &
						Exclamation!, OK!) 
		RETURN True
	ELSE
		istr_mant.argumento[24]	=	ls_codigo
		dw_2.SetItem(1, "embq_nomnav", ls_nombre)
		dw_2.SetItem(1, "puer_codigo", li_Puerto)	
		RETURN False
	END IF
ELSE
	RETURN False
END IF

end function

event open;Integer		li_TipoPlanta
String		ls_Movto, ls_Nulo
 
SetNull(ls_Nulo)

/* Si Código de parempresa es igual a 1 se efectua la transacción para REBAJE DE EXISTENCIA*/
  SELECT empr_coacce
    INTO :ii_controlaaceso  
    FROM dbo.parempresa  
	 USING sqlca;

ii_TipoEnt	=	Integer(Message.StringParm)
ii_Packing	=	Integer(ls_Nulo)

ls_Movto						=	Message.StringParm

ii_TipoEnt					=	Integer(Mid(ls_movto,1,1))

istr_mant.argumento[35]	=	Mid(ls_movto,2,1)
istr_mant.argumento[39]	= ls_movto

IF gi_codexport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("rfpe_ptaori", dw_ptaori)
dw_2.GetChild("puer_codigo", dw_puerto)

dw_planta.SetTransObject(sqlca)
dw_ptaori.SetTransObject(sqlca)
dw_puerto.SetTransObject(sqlca)
dw_planta.Retrieve(1)
dw_ptaori.Retrieve(gi_codplanta)
dw_puerto.Retrieve(900)

Call Super::Open

dw_2.Object.rfpe_tipoen[1] = 3
sqlconec					=	CREATE Transaction
iuo_patente				=	CREATE	uo_patente

IF ii_controlaaceso = 1 THEN
	IF NOT coneccionbase() THEN
		MessageBox("Sin Conexión", "No Existe Conexión a Base Control de Acceso.", StopSign!, Ok!)	
	ELSE
		dw_2.Object.rfpe_patent.Dddw.Name				=	'dw_mues_controlacceso'
		dw_2.Object.rfpe_patent.Dddw.DisplayColumn	=	'ctac_patent'
		dw_2.Object.rfpe_patent.Dddw.DataColumn		=	'ctac_patent'
		dw_2.Object.rfpe_patent.Dddw.AllowEdit			=  True
		dw_2.Object.rfpe_patent.Dddw.HScrollBar		=  True
		dw_2.Object.rfpe_patent.Dddw.VScrollBar		=  True
		dw_2.Object.rfpe_patent.Dddw.Case 				= 	"Upper"
		dw_2.Modify("rfpe_patent.dddw.Limit=10")
		dw_2.Modify("rfpe_patent.Dddw.PercentWidth=125")
	
		dw_2.GetChild("rfpe_patent", idwc_patente)
		idwc_patente.SetTransObject(sqlconec)
		idwc_patente.Retrieve()
	END IF	
END IF

istr_mant.argumento[1]	=	String(gi_codplanta)
istr_mant.argumento[3]	=	String(gi_codexport)
istr_mant.argumento[5]	=	'dw_mues_recfruproced'
istr_mant.argumento[6]	= 	'1'
istr_mant.argumento[13] =	String(li_TipoPlanta)
istr_mant.argumento[20] =  String(ii_TipoEnt)
istr_mant.argumento[21] =	''
istr_mant.argumento[24] =	''
istr_mant.argumento[4]  =	'99'
istr_mant.argumento[11] =	'99'
istr_mant.argumento[31] =	String(ii_TipoEnt)
dw_2.SetItem(1, "rfpe_numero", Now())

istr_mant.argumento[36]	=	''		// Fecha Ingreso para palletfruta

pb_nuevo.PostEvent(Clicked!)

This.Height	=	2500

					

end event

event ue_borra_detalle;call super::ue_borra_detalle;Long		pallet
Integer	li_Cliente,li_Planta

IF dw_1.rowcount() < 2 THEN
	dw_1.SetFocus()
	RETURN
END IF

pallet=dw_1.getitemNumber(dw_1.GetRow(),"paen_numero")

li_Cliente	= dw_1.GetItemNumber(dw_1.GetRow(), "clie_codigo")
li_Planta	= dw_1.GetItemNumber(dw_1.GetRow(), "plde_codigo")

IF ExisteSpro_palletencab(li_Cliente,li_Planta,pallet) THEN
	MessageBox("Atención","No se puede borrar actual registro. Pallet Caja a Caja")	
	RETURN
END IF

IF dw_1.rowcount() < 1 THEN
	dw_1.SetFocus()
	RETURN
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

THIS.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= False //True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		IF Messagebox('Borrar','Elimina Detalle de Pallet ?', question!, yesno!, 2) = 1 THEN
			istr_mant.argumento[36] = '0'
			//pb_grabar.TriggerEvent(Clicked!)
			This.TriggerEvent("ue_guardar")
		END IF 
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_1.RowCount() = 0 THEN 
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;Integer	li_Cajas

istr_mant.Borra		=	False
istr_mant.Agrega		=	True
dw_2.Enabled			=	False
Message.DoubleParm	=	0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN
	
istr_mant.Argumento[36]	=	String(dw_2.Object.rfpe_fecrec[1])
istr_mant.Argumento[37] =  String(dw_2.Object.rfpe_numero[1])

istr_mant.Argumento[40] =  String(dw_2.Object.tica_codigo[1])
istr_mant.Argumento[41] =  String(dw_2.Object.embq_codigo[1])
istr_mant.Argumento[42] =  String(dw_2.Object.puer_codigo[1])
istr_mant.Argumento[43] =  String(dw_2.Object.tran_codigo[1])
istr_mant.Argumento[44] =  String(dw_2.Object.rfpe_patent[1])	
istr_mant.Argumento[45] =  String(dw_2.Object.rfpe_chofer[1])	
istr_mant.Argumento[46] =  String(dw_2.Object.frre_codigo[1])	
istr_mant.Argumento[47] =  dw_2.Object.rfpe_observ[1]

OpenWithParm(w_maed_palletencab_recepcion_devolucion, istr_mant)

IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
	
IF dw_1.RowCount() > 0 AND Not pb_eliminar.Enabled THEN
	pb_eliminar.Enabled	=	True
	pb_grabar.Enabled		=	True
END IF
	
dw_1.SetRow(il_Fila)
dw_1.SelectRow(il_Fila, True)

end event

event ue_recuperadatos;call super::ue_recuperadatos;Long		ll_FilasDet, ll_FilasEnc

dw_1.SetRedraw(False)
dw_2.SetRedraw(False)

ll_FilasEnc		=	dw_2.Retrieve(Integer(istr_mant.argumento[1]), &
											Long(istr_mant.argumento[2]), &
											Integer(istr_mant.argumento[3]))

IF ll_FilasEnc = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Encabezado Inventario de Pallet")

	dw_1.SetRedraw(True)
	dw_2.SetRedraw(True)

	RETURN
ELSEIF ll_FilasEnc > 0 THEN
	istr_Mant.Solo_Consulta	=	False
	istr_mant.argumento[21]	=	String(dw_2.Object.rfpe_ptaori[1])
ELSE
	dw_1.SetRedraw(True)
	dw_2.SetRedraw(True)

	RETURN
END IF
		
ll_FilasDet	=	dw_1.Retrieve(Integer(istr_mant.argumento[1]), &
											Long(istr_mant.argumento[2]), &
											Integer(istr_mant.argumento[3]))
	
IF ll_FilasDet = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Detalle Inventario de Pallet")

	dw_1.SetRedraw(True)
	dw_2.SetRedraw(True)

	RETURN
ELSE
	pb_Eliminar.Enabled  =	Not istr_mant.Solo_Consulta
	pb_Grabar.Enabled		=	Not istr_mant.Solo_Consulta
	pb_ins_det.Enabled	=	Not istr_mant.Solo_Consulta
	//pb_eli_det.Enabled	=	Not istr_mant.Solo_Consulta
		
	IF ll_FilasDet > 0 THEN
		pb_imprimir.Enabled	=	True
		
		dw_1.SetRow(1)
		dw_1.SelectRow(1, True)
		dw_1.SetFocus()
		
		HabilitaEncab(False)
	END IF

	pb_ins_det.SetFocus()
END IF

dw_1.SetRedraw(True)
dw_2.SetRedraw(True)
end event

on w_maed_recfruprocee_particular_devolucion.create
call super::create
end on

on w_maed_recfruprocee_particular_devolucion.destroy
call super::destroy
end on

event ue_nuevo;HabilitaEncab(True)

ib_ok	=	True

CHOOSE CASE wf_modifica()
	CASE -1
		ib_ok	=	False

	CASE 1
		Message.DoubleParm	=	0
		
		This.TriggerEvent("ue_guardar")
		
		IF message.DoubleParm = -1 THEN ib_ok = False
		
	CASE 3
		ib_ok	=	False
		
		RETURN
END CHOOSE

IF Not ib_ok THEN RETURN

dw_1.Reset()

pb_eli_det.Enabled		=	False
pb_ins_det.Enabled		=	False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
istr_mant.solo_consulta	=	False
dw_2.Enabled				=	True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.Object.clie_codigo[1]	=	gi_codexport
dw_2.Object.plde_codigo[1]	=	gi_codplanta


IF ii_TipoEnt = 1 THEN
	dw_2.Object.tica_codigo[1]	=	1
	dw_2.Object.tran_codigo[1]	=	9999
ELSE
	dw_2.Object.tica_codigo[1]	=	3
END IF

IF ii_TipoEnt	=	3	OR ii_TipoEnt	=	5 THEN dw_2.Object.rfpe_ptaori[1]	=	ii_Packing

dw_2.SetItem(1, "rfpe_numero", Now())

dw_2.SetRedraw(True)
dw_2.SetFocus()
end event

event ue_seleccion;call super::ue_seleccion;istr_busq.argum[1]	= istr_mant.argumento[3]	
istr_busq.argum[2]	= istr_mant.argumento[1]	

OpenWithParm(w_busc_recfruprocee_devolucion_puerto, istr_busq)

istr_busq	       = Message.PowerObjectParm

IF istr_busq.argum[5] <> "" THEN
	istr_mant.argumento[1]	= istr_busq.argum[2]
	istr_mant.argumento[2]	= istr_busq.argum[5]
	istr_mant.argumento[3]	= istr_busq.argum[1]
	istr_mant.argumento[4]  = istr_busq.argum[7]
   istr_mant.argumento[20] = istr_busq.argum[17]
	ib_existe_folio	=	True
	This.TriggerEvent("ue_recuperadatos")
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_borrar;call super::ue_borrar;//Long ll_fila, Pallet
//
//IF dw_1.rowcount() >0 THEN
//	FOR ll_fila=1 TO dw_1.rowcount()
//		 Pallet = dw_1.object.paen_numero[ll_fila]
//		 rebajaTamlot(Pallet)
//	NEXT
//END IF

end event

event ue_validaborrar;call super::ue_validaborrar;//Long ll_fila, Pallet
//
//IF dw_1.rowcount() >0 THEN
//	FOR ll_fila=1 TO dw_1.rowcount()
//		 Pallet = dw_1.object.paen_numero[ll_fila]
//		 rebajaTamlot(Pallet)
//	NEXT
//END IF
end event

event ue_guardar;Integer li_Planta, li_Cliente, li_administradora, li_bodeadmin, li_codigo, li_Estado
Long    ll_Pallet, ll_Fila, ll_numero

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN 	RETURN

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

event ue_antesguardar;call super::ue_antesguardar;Long			ll_nrodoc
Integer		li_Cliente, li_Planta

If	ib_Borrar	=	False	Then
	li_Cliente	=	dw_2.Object.clie_codigo[1]
	li_Planta	=	dw_2.Object.plde_codigo[1]
	
	dw_2.Object.rfpe_pcopda[1]	=	2
	dw_2.Object.rfpe_usuari[1] =	gstr_us.Nombre
	dw_2.Object.rfpe_estaci[1] =	gstr_us.Computador 
	
	If dw_2.GetNextModIfied(0, Primary!) > 0 Then
		dw_2.Object.rfpe_fecact[1]	=	Today()
		dw_2.Object.rfpe_horact[1]	=	Now()		
	End If
	//	Determina Correlativo 
	If dw_2.GetItemStatus(1, 0, Primary!) = New! OR &
		dw_2.GetItemStatus(1, 0, Primary!) = NewModIfied! Then			
		//	Obtención de Folio 
		If IsNull(istr_mant.Argumento[2]) OR  Long(istr_mant.Argumento[2]) = 0 Then
			// Bloquea Grabación
			Integer li_expo
			SELECT expo_codigo INTO :li_expo
			FROM dbo.parempresa;
			UPDATE dbo.parempresa SET
			expo_codigo = :li_expo;
	
			ll_nrodoc	=	Buscanuevofolio(Integer(istr_mant.argumento[1]))		
			
			If Long(ll_nrodoc) = 0  Then
				MessageBox("Advertencia","No Quedan Correlativos Disponibles, Proceda por Mantención.")
				Message.DoubleParm = -1
				Return 
			End If
			
			istr_mant.Argumento[2]		=	String(ll_nrodoc)
			dw_2.Object.rfpe_numero[1]	=	ll_nrodoc

			dw_2.Object.rfpe_tipoen[1] = 	3
		End If	
	End If
End If	

end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	pb_Eliminar.Enabled	=	True
	
	dw_2.Enabled			=	Not istr_mant.Solo_Consulta
	pb_Eliminar.Enabled	=	Not istr_mant.Solo_Consulta
	pb_Grabar.Enabled		=	Not istr_mant.Solo_Consulta
	pb_ins_det.Enabled	=	Not istr_mant.Solo_Consulta
	//pb_eli_det.Enabled	=	Not istr_mant.Solo_Consulta
	
	wf_BloqueaColumnas(istr_mant.Solo_Consulta)
ELSE
	pb_ins_det.Enabled	=	Not istr_mant.Solo_Consulta
END IF
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_recfruprocee_particular_devolucion
integer x = 46
integer y = 940
integer width = 3383
integer height = 924
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_recfruproced_caja"
boolean livescroll = false
end type

event dw_1::dragdrop;call super::dragdrop;dw_1.Object.objetname.Moveable = 0 
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_recfruprocee_particular_devolucion
integer x = 37
integer y = 28
integer width = 3109
integer height = 828
string dataobject = "dw_mant_recfruprocee_particular_devolucion"
end type

event dw_2::itemchanged;call super::itemchanged;String	ls_Columna, ls_Nula
Date		ld_Nula
Long		ll_Nulo
Integer	li_Nulo

DataWIndowChild	dw_calibres

SetNull(ls_Nula)
SetNull(ld_Nula)
SetNull(ll_Nulo)
SetNull(li_Nulo)

ls_columna = GetColumnName()

CHOOSE CASE ls_columna
	CASE "clie_codigo"
		istr_mant.argumento[3]	= data
		
		
	CASE "plde_codigo"
		IF Not ExisteFolio(ls_columna, data) THEN
			This.SetItem(1, "plde_codigo", li_Nulo)
			RETURN 1
		END IF
		
		istr_mant.argumento[1]  = data
		
	CASE "embq_codigo"
		IF NoExisteEmbarque(ls_columna, data) THEN
			This.SetItem(1, ls_columna, ls_nula)
			RETURN 1
		END IF	
		
	CASE "rfpe_numero"
		IF Not ExisteFolio(ls_columna, data) THEN
			This.SetItem(1, "rfpe_numero", ll_Nulo)
			RETURN 1
		END IF
		
	CASE "rfpe_tarjas"
		istr_mant.argumento[4]	= data
		
	CASE "rfpe_tardef"
		istr_mant.argumento[11]	= data

//	CASE "rfpe_tipoen"
//		IF (data='1') THEN
//			istr_mant.argumento[31] = '1'
//			istr_mant.argumento[13] = '2'
//			dw_ptaori.Setfilter("plde_tipopl=2")
//			dw_ptaori.Filter()
//		ELSE
//			istr_mant.argumento[31] = '-1'
//		   IF (data='2') OR (data='6') THEN
//			   istr_mant.argumento[13] = '1'
//			   dw_ptaori.Setfilter("plde_tipopl=1")
//			   dw_ptaori.Filter()
//		   END IF
//		istr_mant.argumento[20] = data
//	  END IF
//		
	CASE "rfpe_ptaori"
		IF NoexistePlanta(data) THEN
			This.SetItem(Row, ls_Columna, li_Nulo)
			RETURN 1
		ELSE 
			istr_mant.argumento[21]=data
	END IF
		
		
	CASE "rfpe_fecrec"
//		IF Not f_validafechatempo(date(data)) THEN
//			This.SetItem(Row, ls_Columna, ld_nula)
//			RETURN 1
//		END IF
			istr_mant.argumento[38]=data
		
	CASE "rfpe_nrores"
		istr_mant.argumento[30]	= data	
		

END CHOOSE

HabilitaIngreso(ls_Columna)
end event

event dw_2::clicked;call super::clicked;CHOOSE CASE dwo.name
			
	CASE "buscaproductor"
		BuscaProductor()
		
		CASE "buscaembarque"
		BuscaEmbarque()
		HabilitaIngreso('embq_codigo')
		
END CHOOSE



end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_recfruprocee_particular_devolucion
integer x = 3561
integer y = 288
end type

event pb_nuevo::clicked;call super::clicked;Long		ll_Null

SetNull(ll_Null)

istr_mant.argumento[2]	=	String(ll_Null)
end event

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_recfruprocee_particular_devolucion
boolean visible = false
integer x = 3561
integer y = 468
end type

event pb_eliminar::clicked;ii_borra = 1

call Super::Clicked
end event

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_recfruprocee_particular_devolucion
integer x = 3566
integer y = 652
end type

event pb_grabar::clicked;ib_primera_entrada = False

istr_mant.argumento[35] =	'1'
istr_mant.argumento[36] =	'1'

call super:: clicked

ib_existe_folio = True
end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_recfruprocee_particular_devolucion
boolean visible = false
integer x = 3561
integer y = 832
end type

event pb_imprimir::clicked;Integer		li_TipoEntrada, li_Reimprime
Long			ll_FilaPallet, ll_FilaCajas, ll_NroPallet, ll_NroCajas

li_TipoEntrada	=	dw_2.Object.rfpe_tipoen[1]


end event

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_recfruprocee_particular_devolucion
integer x = 3575
integer y = 996
end type

event pb_salir::clicked;istr_mant.argumento[33] = '0'
istr_mant.argumento[35] = '0'
istr_mant.argumento[36] = '0'

call super:: clicked
end event

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_recfruprocee_particular_devolucion
integer x = 3575
integer y = 1356
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_recfruprocee_particular_devolucion
integer x = 3575
integer y = 1532
end type

event pb_eli_det::clicked;//
end event

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_recfruprocee_particular_devolucion
integer x = 3557
integer y = 108
end type

