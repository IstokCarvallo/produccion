$PBExportHeader$w_maed_recfruprocee_gennueva.srw
forward
global type w_maed_recfruprocee_gennueva from w_mant_encab_deta
end type
type dw_11 from datawindow within w_maed_recfruprocee_gennueva
end type
type dw_spro_palletencab from datawindow within w_maed_recfruprocee_gennueva
end type
type dw_palletencab from datawindow within w_maed_recfruprocee_gennueva
end type
type dw_palletfruta from datawindow within w_maed_recfruprocee_gennueva
end type
type dw_spro_cajasprodpallet from datawindow within w_maed_recfruprocee_gennueva
end type
type dw_spro_palletfruta from datawindow within w_maed_recfruprocee_gennueva
end type
type pb_captura from picturebutton within w_maed_recfruprocee_gennueva
end type
type dw_3 from datawindow within w_maed_recfruprocee_gennueva
end type
end forward

global type w_maed_recfruprocee_gennueva from w_mant_encab_deta
integer width = 3566
integer height = 1988
string title = "RECEPCION DE PALLETS"
string menuname = ""
boolean minbox = false
boolean maxbox = false
boolean resizable = false
event ue_imprimir ( )
event ue_validaregistro ( )
event ue_despuesguardar ( )
dw_11 dw_11
dw_spro_palletencab dw_spro_palletencab
dw_palletencab dw_palletencab
dw_palletfruta dw_palletfruta
dw_spro_cajasprodpallet dw_spro_cajasprodpallet
dw_spro_palletfruta dw_spro_palletfruta
pb_captura pb_captura
dw_3 dw_3
end type
global w_maed_recfruprocee_gennueva w_maed_recfruprocee_gennueva

type variables
w_mant_deta_recfruprocee iw_mantencion

DataWindowChild	dw_puerto, dw_planta, dw_ptaori, dw_fruta, idwc_patente, dw_ptaori2
Integer 	ii_recepcion, ii_cliente, ii_planta, ii_estado, ii_controlaaceso,ii_borra=0,ii_bloquea
Boolean	ib_existe_folio, ib_primera_entrada, ib_conectado
Long     il_pallet, il_folio, il_numero, il_NroCaja, il_NroPallet
Date     id_mespro 
String   is_Archivo, is_mensaje, is_Computador, is_correo, is_nomplanta, is_planta

Boolean	ib_ConectadoExistencia


uo_pallet				iuo_pallet
uo_especie				iuo_especie
uo_variedades			iuo_variedades
uo_embalajesprod		iuo_embalajesprod
uo_etiquetas			iuo_etiquetas
uo_tipofrio				iuo_tipofrio	
uo_status				iuo_status	
uo_tipopallet			iuo_tipopallet	
uo_condicion			iuo_condicion	
uo_codigopallet		iuo_codigopallet	
uo_destinos				iuo_destinos
uo_productores			iuo_productores	
uo_calibre				iuo_calibre
uo_categoria         iuo_categoria
uo_tratamiento       iuo_tratamiento
uo_frutarecepcion		iuo_frutarecepcion
uo_patente				iuo_patente
uo_lotescorrelequipo	iuo_correl
uo_responablescierre	iuo_responablescierre


DataStore	ids_palletfruta_fechaI
end variables

forward prototypes
public function boolean noexistecliente (integer ai_codigo)
public subroutine habilitaingreso ()
public function long buscanuevofolio (integer cliente, integer planta)
public function boolean noexisteembarque (string as_columna, string as_valor)
public subroutine habilitaencab (boolean habilita)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean noexisteplanta (string columna)
public function boolean existefolio (string as_columna, string as_valor)
public function integer cajaspallet (string as_embalaje)
public function long buscafoliorecfruprocee_trans (integer ai_planta)
public function string buscdescfruta (integer fruta)
public function boolean grabadocrelhisto ()
public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente)
public function boolean existepallet (integer ai_cliente, integer ai_planta, long al_pallet)
public function boolean existe_palletinterplanta (integer ai_cliente, integer ai_planta, long al_pallet)
end prototypes

event ue_imprimir;istr_mant.argumento[38] = String(dw_2.Object.frre_codigo[1])
istr_mant.argumento[41] = String(dw_2.Object.rfpe_fecrec[1])

OpenWithParm(w_info_recepciones_detalle, istr_mant)

//SetPointer(HourGlass!)
//Long		fila
//Date		ld_desde, ld_hasta
//String   ls_recepcion, ls_descri
//
//str_info	lstr_info
//
//SELECT pate_inicio,pate_termin
//INTO   :ld_desde,:ld_hasta
//FROM dba.paramtemporada
//WHERE pate_tempor=1;
//
//lstr_info.titulo	= "RECEPCION DE PALLETS"
//lstr_info.copias	= 1
//
//OpenWithParm(vinf,lstr_info)
//
//vinf.dw_1.DataObject = "dw_info_recepcion_pallet"
////vinf.dw_1.DataObject = "dw_info_recfruproced"
//
//vinf.dw_1.SetTransObject(sqlca)
//
//istr_mant.argumento[30] = String(dw_2.Object.rfpe_numero[1])
//ls_descri = buscdescfruta(dw_2.Object.frre_codigo[1])
//
//ls_recepcion = "Recepcion " +String(dw_2.Object.rfpe_numero[1])
//ls_recepcion = ls_recepcion + "                       Fecha "+String(dw_2.Object.rfpe_fecrec[1])
//ls_recepcion = ls_recepcion + "                       Guia "+String(dw_2.Object.rfpe_nrores[1])
//ls_recepcion = ls_recepcion + "                       En Planta "+String(dw_2.Object.plde_codigo[1])
//
//fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]), &
//ld_desde,ld_hasta,0,Long(istr_mant.argumento[30]),0,0)
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
//  	vinf.dw_1.Modify("guia.text = '" + ls_recepcion + "'")
//	vinf.dw_1.Modify("fruta.text = '" + ls_descri + "'")  
//	vinf.Visible	= True
//	vinf.Enabled	= True
//END IF
//SetPointer(Arrow!)
end event

public function boolean noexistecliente (integer ai_codigo);String	ls_nombre

SELECT	clie_nombre
INTO	:ls_nombre  
	FROM	dba.clientesprod  
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

public subroutine habilitaingreso ();Date		ld_fecha
Integer	li_tarjas,li_tardef
Boolean	lb_estado = True
String	ls_patent, ls_chofer

dw_2.AcceptText()

li_tarjas	=	dw_2.Object.rfpe_tarjas[1]
li_tardef	=	dw_2.Object.rfpe_tardef[1]
ls_patent	=	dw_2.Object.rfpe_patent[1]
ls_chofer	=	dw_2.Object.rfpe_chofer[1]

IF IsNull(li_tarjas) THEN li_tarjas = 0
IF IsNull(li_tardef) THEN li_tardef = 0

//IF IsNull(dw_2.Object.rfpe_numero[1]) OR dw_2.Object.rfpe_numero[1] = 0 OR &
IF IsNull(dw_2.Object.rfpe_nrores[1]) OR dw_2.Object.rfpe_nrores[1] = 0 OR &
	IsNull(dw_2.Object.tica_codigo[1]) OR dw_2.Object.tica_codigo[1] = 0 OR &
	IsNull(ls_patent) OR ls_patent = "" OR &
	IsNull(dw_2.Object.tran_codigo[1]) OR dw_2.Object.tran_codigo[1] = 0 OR &
	IsNull(ls_chofer) OR ls_chofer = "" OR &
	li_tarjas + li_tardef = 0 THEN
	lb_estado = False
END IF

CHOOSE CASE dw_2.Object.rfpe_tipoen[1]
	CASE 1, 2
		IF IsNull(dw_2.Object.rfpe_ptaori[1]) OR dw_2.Object.rfpe_ptaori[1] = 0 THEN
			lb_estado = False
		END IF
	
	 CASE 3
		IF IsNull(dw_2.Object.puer_codigo[1]) OR dw_2.Object.puer_codigo[1] = 0 OR &
			IsNull(dw_2.Object.embq_codigo[1]) OR dw_2.Object.embq_codigo[1] = "" THEN
			lb_estado = False
		END IF			
END CHOOSE	

pb_ins_det.Enabled = lb_estado
end subroutine

public function long buscanuevofolio (integer cliente, integer planta);Integer	li_planta, li_tipoins, li_movto
Long		ll_numero,ll_numero2, ll_fin, ll_actual

li_planta	=	planta

li_movto = 1

SELECT Max(rfpe_numero)
INTO :ll_numero
FROM DBA.RECFRUPROCEE
WHERE plde_codigo = :li_planta;
 
SELECT como_inicia, como_actual, como_termin
INTO	:ll_numero2, :ll_actual, :ll_fin
FROM DBA.CORRELMOVIMIENTOS 
WHERE plde_codigo = :li_planta
AND	COMO_TIPOMV = :li_movto;

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
		FROM	dba.embarqueprod
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

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.SetTabOrder("clie_codigo",10)
	dw_2.SetTabOrder("rfpe_numero",20)
	dw_2.SetTabOrder("plde_codigo",30)
	dw_2.SetTabOrder("rfpe_fecrec",0)
	dw_2.SetTabOrder("rfpe_horrec",0)
	dw_2.SetTabOrder("inpr_numero",0)   
	dw_2.SetTabOrder("rfpe_tarjas",0)   
	//dw_2.SetTabOrder("rfpe_nrores",0)   
	dw_2.SetTabOrder("rfpe_tipoen",0)   
	dw_2.SetTabOrder("rfpe_guides",0)   
	dw_2.SetTabOrder("rfpe_ptaori",0)   
	dw_2.SetTabOrder("puer_codigo",0)   
	dw_2.SetTabOrder("prod_codigo",0)   
	dw_2.SetTabOrder("rfpe_nomres",0)   
  	dw_2.SetTabOrder("rfpe_tardef",0)   
	dw_2.SetTabOrder("tran_codigo",0)   
	dw_2.SetTabOrder("rfpe_patent",0)   
	dw_2.SetTabOrder("rfpe_chofer",0)   
	dw_2.SetTabOrder("tica_codigo",0)   
	dw_2.SetTabOrder("rfpe_fecact",0)   
	dw_2.SetTabOrder("rfpe_horact",0)   
	dw_2.SetTabOrder("frre_codigo",0)   
	dw_2.SetTabOrder("rfpe_fecing",0)   
	dw_2.SetTabOrder("rfpe_horing",0)   
	dw_2.SetTabOrder("rfpe_sucuco",0)   
	dw_2.SetTabOrder("rfpe_usuari",0)   
	dw_2.SetTabOrder("rfpe_estaci",0)   
	dw_2.SetTabOrder("rfpe_pcopda",0) 
		
	dw_2.Modify("clie_codigo.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("rfpe_numero.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("rfpe_fecrec.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_horrec.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("inpr_numero.BackGround.Color = " + String(rgb(255,255,255)))
	dw_2.Modify("rfpe_tarjas.BackGround.Color = " + String(RGB(166,180,210)))
	//dw_2.Modify("rfpe_nrores.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_tipoen.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_guides.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_ptaori.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("puer_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("prod_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_nomres.BackGround.Color = " + String(RGB(166,180,210)))
  	dw_2.Modify("rfpe_tardef.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("tran_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_patent.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_chofer.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("tica_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_fecact.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_horact.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("frre_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_fecing.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_horing.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_sucuco.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_usuari.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_estaci.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_pcopda.BackGround.Color = " + String(RGB(166,180,210)))
		
	dw_2.SetColumn("rfpe_numero")
	dw_2.SetFocus()
	
	ii_bloquea = 1
ELSE
	dw_2.SetTabOrder("clie_codigo",0)
	dw_2.SetTabOrder("rfpe_numero",0)
	dw_2.SetTabOrder("plde_codigo",0)
	dw_2.SetTabOrder("rfpe_fecrec",0)
	dw_2.SetTabOrder("rfpe_horrec",0)
	dw_2.Modify("clie_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_numero.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("plde_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_fecrec.BackGround.Color = " + String(RGB(166,180,210)))
	dw_2.Modify("rfpe_horrec.BackGround.Color = " + String(RGB(166,180,210)))
	
	ii_bloquea = 1

END IF
end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno
Long numero
Integer li_planta, li_movto

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	IF dw_1.Update() = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
	ELSEIF dw_2.Update() = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	ELSE
		Commit;
		
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
		ELSE
			lb_Retorno	=	True
		END IF
	END IF
ELSE
	IF NOT isnull(dw_3.Object.rfpe_numero[1]) AND dw_3.Object.rfpe_numero[1] <> 0 THEN
		IF dw_3.Update() = -1 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
			RollBack;
		END IF
	END IF
	
	IF dw_2.Update() = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		RollBack;
	END IF
	
	IF dw_1.Update() = -1 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	ELSE
		dw_1.ResetUpdate()
		dw_2.ResetUpdate()
		dw_3.ResetUpdate()
		COMMIT;
	END IF	
END IF

Commit;
 
sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean noexisteplanta (string columna);Integer	li_cliente, li_planta, li_tipo

li_cliente	=	Integer(istr_mant.argumento[3])
li_planta	=	Integer(columna)
li_tipo		=	Integer(istr_mant.argumento[13])

SELECT	plde_codigo
INTO	:li_planta
FROM	dba.plantadesp  
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

public function boolean existefolio (string as_columna, string as_valor);Integer	li_planta, li_existe,li_cliente, li_tipoen
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
FROM	dba.RECFRUPROCEE
	WHERE	plde_codigo	=	:li_planta
	AND	rfpe_numero	=	:ll_nfolio
	AND   clie_codigo =  :li_cliente; 
			
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla RecFruProcee")
	RETURN False
ELSEIF sqlca.SQLCode = 0 THEN
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
	RETURN False
ELSE
	IF IsNull(ll_nfolio) THEN
		istr_mant.argumento[1]	= String(li_planta)
		istr_mant.argumento[2]	= String(ll_nfolio)
		istr_mant.argumento[3]	= String(li_cliente)
		ib_existe_folio	=	False
		RETURN False
	ELSE
		MessageBox("Atención","Número de Documento No ha sido generado. Ingrese Otro.")
		ib_existe_folio	=	False
		RETURN True
	END IF
END IF

end function

public function integer cajaspallet (string as_embalaje);Integer	li_CajasPallet, li_Cliente

li_Cliente = dw_2.Object.clie_codigo[1]

SELECT	emba_cajpal
INTO	:li_CajasPallet
FROM	dba.embalajesprod
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

public function long buscafoliorecfruprocee_trans (integer ai_planta);Integer	li_planta, li_cliente
Long		ll_numero
Boolean	lb_nulo

//li_cliente	=	cliente	
li_planta	=	ai_planta

SELECT Max(rfpe_numero)
	INTO :ll_numero
	FROM DBA.RECFRUPROCEE_TRANS
	WHERE plde_codigo = :li_planta
	USING sqlca ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla RecFruProcee_trans")
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

public function string buscdescfruta (integer fruta);Integer	li_codigo
String	ls_descri, ls_abrevi

SELECT frre_codigo,frre_descri,frre_abrevi  
INTO :li_codigo,:ls_descri,:ls_abrevi  
FROM dba.frutarecibida  
WHERE frre_codigo = :fruta;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla frutarecibida")
END IF
	
RETURN ls_descri

end function

public function boolean grabadocrelhisto ();String	ls_embala
Integer	li_cliente, li_cancaj, li_planta
Long		ll_pallet, ll_docrel

li_cliente	= Integer(istr_mant.argumento[3])
li_planta	= Integer(istr_mant.argumento[1])
ll_pallet   = Long(istr_mant.argumento[6])

SELECT	DISTINCT pafr_docrel
	INTO	:ll_docrel
	FROM	dba.spro_palletfruta
	WHERE	clie_codigo	=	:li_Cliente
	AND	paen_numero	=	:ll_pallet
	AND	plde_codigo	=	:li_planta;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_palletfruta")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	//MessageBox("Atención","Tipo de Embalaje no Existe para este Cliente.~rIngrese Otro.", Exclamation!, Ok!)
	RETURN False
ELSE
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla palletfruta")
		RETURN False
	END IF
		
	UPDATE dba.palletfrutahisto SET
		pafr_docrel = :ll_docrel
		WHERE clie_codigo = :li_cliente
		AND	plde_codigo = :li_planta
		AND	paen_numero = :ll_pallet;
		
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla palletfrutahisto")
		RETURN False
	END IF
	
	Commit;
	
	RETURN True
END IF

RETURN False
end function

public function boolean buscanuevocorrelativo (integer ai_bodega, integer ai_cliente);IF il_NroCaja < 1 OR IsNull(il_NroCaja) THEN

	IF NOT iuo_correl.Existe(ii_Planta,99, is_Computador, TRUE, sqlca) THEN
		SetNull(il_NroCaja)
		RETURN FALSE
	ELSE
		il_NroCaja	=	iuo_correl.il_correcompa
	END IF

ELSE
	//il_NroCaja = il_NroCaja + 1
END IF

//dw_1.Object.capr_numero[1]	=	il_NroCaja

RETURN True
end function

public function boolean existepallet (integer ai_cliente, integer ai_planta, long al_pallet);long ll_count
  
SELECT Count()
INTO :ll_count  
FROM dba.palletencab
	WHERE clie_codigo = :ai_cliente
	AND plde_codigo = :ai_planta
	AND paen_numero = :al_pallet  
	USING sqlca   ;
	 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla Palletencab")
	RETURN True
ELSEIF ll_count > 0 THEN
	RETURN True

END IF


RETURN False


end function

public function boolean existe_palletinterplanta (integer ai_cliente, integer ai_planta, long al_pallet);long ll_count
  
SELECT Count()
INTO :ll_count  
FROM dba.palletencab
	WHERE clie_codigo = :ai_cliente
	AND plde_codigo = :ai_planta
	AND paen_numero = :al_pallet 
	USING sqlca   ;
	 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla Palletencab")
	RETURN True
ELSEIF ll_count > 0 THEN
	RETURN True

END IF


RETURN False


end function

event open;//	Argumentos Mantenedor

Integer li_codigo

IF Not f_validafechatempo(today()) THEN
   Messagebox('Atención','Aclare Fecha Actual Temporada con Informática')
END IF

IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_3.GetChild("plde_codigo", dw_planta)
dw_3.GetChild("rfpe_ptaori", dw_ptaori)
dw_3.GetChild("puer_codigo", dw_puerto)

dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("rfpe_ptaori", dw_ptaori2)
dw_2.GetChild("puer_codigo", dw_puerto)

dw_planta.SetTransObject(sqlca)
dw_ptaori.SetTransObject(sqlca)
dw_puerto.SetTransObject(sqlca)
dw_ptaori2.SetTransObject(sqlca)

dw_planta.Retrieve(1)
dw_ptaori.Retrieve()
dw_ptaori2.Retrieve()
//dw_ptaori.Setfilter("plde_tipopl=2")
//dw_ptaori.Filter()
dw_puerto.Retrieve(900)

dw_ptaori.SetSort("plde_nombre")
dw_ptaori.Sort( )

dw_2.GetChild("frre_codigo", dw_fruta)
dw_3.GetChild("frre_codigo", dw_fruta)
dw_fruta.SetTransObject(sqlca)
dw_fruta.Retrieve()

x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				= dw_1
istr_mant.solo_consulta = False

buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"

istr_mant.argumento[1]	=	String(gi_codplanta)
istr_mant.argumento[3]	=	String(gi_codexport)
istr_mant.argumento[5]	=	'dw_mues_recfruproced'
istr_mant.argumento[6]	= 	'1'
istr_mant.argumento[13] =	'2'
istr_mant.argumento[20] =	'1'			//tipo recepcion
istr_mant.argumento[21] =	''
istr_mant.argumento[24] =	''
istr_mant.argumento[4]  =	'0'
istr_mant.argumento[11] =	'0'
istr_mant.argumento[31] =	'1'
istr_mant.argumento[32] =	''
istr_mant.argumento[33] =	''
istr_mant.argumento[34] =	''
istr_mant.argumento[35] =	'0'
istr_mant.argumento[36] =	'0'
istr_mant.argumento[39] =	''				// Tipo de Entrada
istr_mant.argumento[40] =	''				// Tipo de Entrada InterPlanta 2 - 6
istr_mant.argumento[41] =  ''
istr_mant.argumento[50] =  '1'			//tipo recepcion

iuo_especie				=	CREATE	uo_especie				
iuo_variedades			=	CREATE	uo_variedades			
iuo_embalajesprod		=  CREATE	uo_embalajesprod		
iuo_etiquetas			=	CREATE	uo_etiquetas			
iuo_tipofrio			=	CREATE	uo_tipofrio					
iuo_status				=	CREATE	uo_status				
iuo_tipopallet			=	CREATE	uo_tipopallet			
iuo_condicion			=	CREATE	uo_condicion				
iuo_codigopallet		=	CREATE	uo_codigopallet			
iuo_destinos			=	CREATE	uo_destinos				
iuo_productores		=	CREATE	uo_productores				
iuo_calibre				=	CREATE	uo_calibre	
iuo_categoria			=	CREATE	uo_categoria
iuo_tratamiento		=	CREATE	uo_tratamiento
iuo_frutarecepcion	=	CREATE	uo_frutarecepcion	
iuo_patente				=	CREATE	uo_patente
iuo_responablescierre	=	CREATE uo_responablescierre

pb_nuevo.PostEvent(Clicked!)

ib_primera_entrada = True

iuo_pallet       = CREATE   uo_pallet

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							



end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
												Information!, RetryCancel!)
			ELSE                                                    
				pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_imprimir.Enabled	= True
				pb_ins_det.Enabled	= True
				pb_grabar.Enabled	= True

				IF ll_fila_d > 0 THEN
					pb_eli_det.Enabled	= True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
								
				ELSE
//	   		pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

pb_eliminar.Enabled		= False
pb_grabar.Enabled			= True
pb_eli_det.Enabled		= False
pb_ins_det.Enabled		= False

IF respuesta = 2 THEN Close(This)
end event

on w_maed_recfruprocee_gennueva.create
int iCurrent
call super::create
this.dw_11=create dw_11
this.dw_spro_palletencab=create dw_spro_palletencab
this.dw_palletencab=create dw_palletencab
this.dw_palletfruta=create dw_palletfruta
this.dw_spro_cajasprodpallet=create dw_spro_cajasprodpallet
this.dw_spro_palletfruta=create dw_spro_palletfruta
this.pb_captura=create pb_captura
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_11
this.Control[iCurrent+2]=this.dw_spro_palletencab
this.Control[iCurrent+3]=this.dw_palletencab
this.Control[iCurrent+4]=this.dw_palletfruta
this.Control[iCurrent+5]=this.dw_spro_cajasprodpallet
this.Control[iCurrent+6]=this.dw_spro_palletfruta
this.Control[iCurrent+7]=this.pb_captura
this.Control[iCurrent+8]=this.dw_3
end on

on w_maed_recfruprocee_gennueva.destroy
call super::destroy
destroy(this.dw_11)
destroy(this.dw_spro_palletencab)
destroy(this.dw_palletencab)
destroy(this.dw_palletfruta)
destroy(this.dw_spro_cajasprodpallet)
destroy(this.dw_spro_palletfruta)
destroy(this.pb_captura)
destroy(this.dw_3)
end on

event ue_nuevo;Integer li_codigo

HabilitaEncab(True)

dw_1.Reset()

pb_eli_det.Enabled		= False
pb_ins_det.Enabled		= False
pb_grabar.Enabled			= False
pb_eliminar.Enabled		= False
pb_imprimir.Enabled		= False
istr_mant.solo_consulta	= False
dw_2.Enabled				= True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_codexport)
dw_2.SetItem(1, "plde_codigo", gi_codplanta)

dw_2.Modify("rfpe_tipoen.BackGround.Color = " + String(RGB(166,180,210)))

dw_ptaori.SetSort("plde_nombre")
dw_ptaori.Sort( )

dw_2.SetRedraw(True)

dw_2.SetFocus()

dw_3.Enabled				= False
dw_3.Reset()
dw_3.InsertRow(0)
dw_3.SetItem(1, "clie_codigo", gi_codexport)
dw_3.SetItem(1, "plde_codigo", gi_codplanta)

dw_3.SetTabOrder("clie_codigo",0)
dw_3.SetTabOrder("rfpe_numero",0)
dw_3.SetTabOrder("plde_codigo",0)
dw_3.SetTabOrder("rfpe_tipoen",0)

dw_3.Modify("rfpe_tipoen.BackGround.Color = " + String(RGB(166,180,210)))
dw_3.Modify("clie_codigo.BackGround.Color = " + String(RGB(166,180,210)))
dw_3.Modify("plde_codigo.BackGround.Color = " + String(RGB(166,180,210)))
dw_3.Modify("rfpe_numero.BackGround.Color = " + String(RGB(166,180,210)))

dw_ptaori.SetSort("plde_nombre")
dw_ptaori.Sort( )
dw_2.SetRedraw(True)


end event

event ue_seleccion;call super::ue_seleccion;istr_busq.argum[1]	= istr_mant.argumento[3]	
istr_busq.argum[2]	= istr_mant.argumento[1]	

OpenWithParm(w_busc_recfruprocee, istr_busq)

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

event resize;//
Integer	maximo, li_posic_x, li_posic_y, li_visible = 0

IF dw_2.width > il_AnchoDw_1 THEN
	maximo		=	dw_2.width
ELSE
	dw_1.width	=	This.WorkSpaceWidth() - 400
	maximo		=	dw_1.width
END IF

dw_2.x					= 37 + Round((maximo - dw_2.width) / 2, 0)
dw_2.y					= 37

//dw_1.x					= 37 + Round((maximo - dw_1.width) / 2, 0)
//dw_1.y					= 64 + dw_2.Height
//dw_1.height				= This.WorkSpaceHeight() - dw_1.y - 300//41

//gb_1.x 					= This.WorkSpaceWidth() - 310
//gb_1.y 					= 0
//gb_1.width				= 275

li_posic_x				= This.WorkSpaceWidth() - 250
li_posic_y				= 275 +  88

IF pb_buscar.Visible THEN
	pb_buscar.x				= li_posic_x
	pb_buscar.y				= li_posic_y
	pb_buscar.width		= 233
	pb_buscar.height		= 196	
	li_visible ++
	li_posic_y += 195
END IF

IF pb_nuevo.Visible THEN
	pb_nuevo.x				= li_posic_x
	pb_nuevo.y				= li_posic_y
	pb_nuevo.width			= 233
	pb_nuevo.height		= 196
	li_visible ++
	li_posic_y += 195
END IF

IF	pb_eliminar.Visible THEN
	pb_eliminar.x			= li_posic_x
	pb_eliminar.y			= li_posic_y
	pb_eliminar.width		= 233
	pb_eliminar.height	= 196
	li_visible ++
	li_posic_y += 195
END IF

IF pb_grabar.Visible THEN
	pb_grabar.x				= li_posic_x
	pb_grabar.y				= li_posic_y
	pb_grabar.width		= 233
	pb_grabar.height		= 196
	li_visible ++
	li_posic_y += 195
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			= li_posic_x
	pb_imprimir.y			= li_posic_y
	pb_imprimir.width		= 233
	pb_imprimir.height	= 196
	li_visible ++
	li_posic_y += 195
END IF

IF pb_captura.Visible THEN
	pb_captura.x			= li_posic_x
	pb_captura.y			= li_posic_y
	pb_captura.width		= 233
	pb_captura.height	= 196
	li_visible ++
	li_posic_y += 195
END IF

IF pb_salir.Visible THEN
	pb_salir.x				= li_posic_x
	pb_salir.y				= li_posic_y
	pb_salir.width			= 233
	pb_salir.height		= 196
	li_visible ++
	li_posic_y += 195
END IF

pb_ins_det.x			= li_posic_x
pb_ins_det.y			= 1300
pb_ins_det.width		= 233
pb_ins_det.height		= 196

pb_eli_det.x			= li_posic_x
pb_eli_det.y			= pb_ins_det.y + 195
pb_eli_det.width		= 233
pb_eli_det.height		= 196
end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
//	pb_Grabar.Enabled		=	False
//	pb_Eliminar.Enabled	=	True
	
	IF istr_mant.Solo_Consulta THEN
//		dw_2.Enabled			=	False
////		pb_Eliminar.Enabled	=	False
//		pb_Grabar.Enabled		=	False
//		pb_ins_det.Enabled	=	False
//		pb_eli_det.Enabled	=	False
	ELSE
//		dw_2.Enabled			=	True
//		pb_Eliminar.Enabled	=	True
//		pb_Grabar.Enabled		=	True
//		pb_ins_det.Enabled	=	True
//		pb_eli_det.Enabled	=	True
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

type dw_1 from w_mant_encab_deta`dw_1 within w_maed_recfruprocee_gennueva
integer x = 37
integer y = 416
integer width = 3177
integer height = 988
integer taborder = 100
string title = "Detalle de Pallets a Replicar"
string dataobject = "dw_mues_recfruproced_gennueva"
boolean livescroll = false
end type

type dw_2 from w_mant_encab_deta`dw_2 within w_maed_recfruprocee_gennueva
integer x = 50
integer y = 12
integer width = 3154
integer height = 368
string dataobject = "dw_mant_recfruprocee_gennueva"
end type

event dw_2::itemchanged;call super::itemchanged;String	ls_columna, ls_nula
Date		ld_nula

DataWIndowChild	dw_calibres

SetNull(ls_nula)
SetNull(ld_nula)

ls_columna = GetColumnName()

CHOOSE CASE ls_columna
	CASE "plde_codigo"
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(1, ls_columna, Integer(ls_nula))
			RETURN 1
		END IF
		
	CASE "rfpe_numero"
		IF ExisteFolio(ls_columna, data) THEN
			This.SetItem(1, ls_columna, Integer(ls_nula))
			RETURN 1
		END IF
	
	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, gi_codexport)
			RETURN 1
		END IF	
	END CHOOSE




end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta`pb_nuevo within w_maed_recfruprocee_gennueva
integer x = 3310
integer y = 260
end type

type pb_eliminar from w_mant_encab_deta`pb_eliminar within w_maed_recfruprocee_gennueva
boolean visible = false
integer x = 3310
integer y = 432
end type

event pb_eliminar::clicked;ii_borra = 1

call Super::Clicked
end event

type pb_grabar from w_mant_encab_deta`pb_grabar within w_maed_recfruprocee_gennueva
integer x = 3310
integer y = 612
end type

type pb_imprimir from w_mant_encab_deta`pb_imprimir within w_maed_recfruprocee_gennueva
boolean visible = false
integer x = 3310
integer y = 792
end type

type pb_salir from w_mant_encab_deta`pb_salir within w_maed_recfruprocee_gennueva
integer x = 3310
integer y = 1332
end type

type pb_ins_det from w_mant_encab_deta`pb_ins_det within w_maed_recfruprocee_gennueva
boolean visible = false
integer x = 3333
integer y = 1656
integer weight = 400
fontcharset fontcharset = ansi!
end type

type pb_eli_det from w_mant_encab_deta`pb_eli_det within w_maed_recfruprocee_gennueva
boolean visible = false
integer x = 3333
integer y = 1832
end type

type pb_buscar from w_mant_encab_deta`pb_buscar within w_maed_recfruprocee_gennueva
integer x = 3310
integer y = 72
end type

type dw_11 from datawindow within w_maed_recfruprocee_gennueva
boolean visible = false
integer x = 343
integer y = 3060
integer width = 2496
integer height = 972
integer taborder = 160
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_recupera_palletinspec_trans"
boolean controlmenu = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_spro_palletencab from datawindow within w_maed_recfruprocee_gennueva
boolean visible = false
integer x = 1102
integer y = 2684
integer width = 352
integer height = 296
integer taborder = 130
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_spro_palletencab"
boolean controlmenu = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
end type

type dw_palletencab from datawindow within w_maed_recfruprocee_gennueva
boolean visible = false
integer x = 366
integer y = 2676
integer width = 352
integer height = 296
integer taborder = 140
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_palletencab_caja"
boolean controlmenu = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
end type

type dw_palletfruta from datawindow within w_maed_recfruprocee_gennueva
boolean visible = false
integer y = 2672
integer width = 352
integer height = 296
integer taborder = 150
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_palletfruta"
boolean controlmenu = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
end type

type dw_spro_cajasprodpallet from datawindow within w_maed_recfruprocee_gennueva
boolean visible = false
integer x = 727
integer y = 2680
integer width = 361
integer height = 300
integer taborder = 130
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_cajasprodpallet"
boolean controlmenu = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
end type

type dw_spro_palletfruta from datawindow within w_maed_recfruprocee_gennueva
boolean visible = false
integer x = 1463
integer y = 2684
integer width = 352
integer height = 296
integer taborder = 140
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_spro_palletfruta"
boolean controlmenu = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
end type

type pb_captura from picturebutton within w_maed_recfruprocee_gennueva
string tag = "Replica Datos"
integer x = 3310
integer y = 1152
integer width = 233
integer height = 196
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 12\Imagenes\Botones\BuscaArch.png"
alignment htextalign = left!
end type

event clicked;Long 		ll_nuevofolio, ll_nro_lote, ll_Pallet
Integer	li_fillas, ll_filas, ll_fila_d, ll_fila_g, li_Cliente, li_Planta
Date ld_Fecha_Ing

dw_3.Enabled				= True

dw_3.Object.plde_codigo[1] = dw_2.Object.plde_codigo[1]   
dw_3.Object.rfpe_numero[1] = 0  
dw_3.Object.inpr_numero[1] = dw_2.Object.inpr_numero[1]   
dw_3.Object.rfpe_fecrec[1] = dw_2.Object.rfpe_fecrec[1]   
dw_3.Object.rfpe_tarjas[1] = dw_2.Object.rfpe_tarjas[1]   
dw_3.Object.rfpe_nrores[1] = dw_2.Object.rfpe_nrores[1]    
dw_3.Object.rfpe_tipoen[1] = dw_2.Object.rfpe_tipoen[1]   
dw_3.Object.rfpe_guides[1] = dw_2.Object.rfpe_guides[1]   
dw_3.Object.rfpe_ptaori[1] = dw_2.Object.rfpe_ptaori[1]   
dw_3.Object.puer_codigo[1] = dw_2.Object.puer_codigo[1]   
dw_3.Object.embq_codigo[1] = dw_2.Object.embq_codigo[1]   
dw_3.Object.prod_codigo[1] = dw_2.Object.prod_codigo[1]   
dw_3.Object.rfpe_nomres[1] = dw_2.Object.rfpe_nomres[1]   
dw_3.Object.clie_codigo[1] = dw_2.Object.clie_codigo[1]   
dw_3.Object.rfpe_tardef[1] = dw_2.Object.rfpe_tardef[1]    
dw_3.Object.tran_codigo[1] = dw_2.Object.tran_codigo[1]   
dw_3.Object.rfpe_patent[1] = dw_2.Object.rfpe_patent[1]   
dw_3.Object.rfpe_chofer[1] = dw_2.Object.rfpe_chofer[1]   
dw_3.Object.tica_codigo[1] = dw_2.Object.tica_codigo[1]   
dw_3.Object.rfpe_fecact[1] = dw_2.Object.rfpe_fecact[1]   
dw_3.Object.rfpe_horact[1] = dw_2.Object.rfpe_horact[1]   
dw_3.Object.frre_codigo[1] = dw_2.Object.frre_codigo[1]   
dw_3.Object.rfpe_horrec[1] = dw_2.Object.rfpe_horrec[1]   
dw_3.Object.rfpe_fecing[1] = dw_2.Object.rfpe_fecing[1]   
dw_3.Object.rfpe_horing[1] = dw_2.Object.rfpe_horing[1]   
dw_3.Object.rfpe_sucuco[1] = dw_2.Object.rfpe_sucuco[1]   
dw_3.Object.rfpe_usuari[1] = dw_2.Object.rfpe_usuari[1]   
dw_3.Object.rfpe_estaci[1] = dw_2.Object.rfpe_estaci[1]   
dw_3.Object.rfpe_pcopda[1] = dw_2.Object.rfpe_pcopda[1]
dw_3.Object.rfpe_numtra[1] = dw_2.Object.rfpe_numtra[1]

ll_nuevofolio = dw_3.Object.rfpe_numero[1]

dw_2.SetTabOrder("rfpe_nrores",10)
dw_3.Modify("rfpe_nrores.BackGround.Color = " + String(rgb(255,255,255)))

dw_2.SetTabOrder("clie_codigo",0)
dw_2.SetTabOrder("rfpe_numero",0)
dw_2.SetTabOrder("plde_codigo",0)

dw_3.Modify("rfpe_tipoen.BackGround.Color = " + String(RGB(166,180,210)))
dw_3.Modify("clie_codigo.BackGround.Color = " + String(RGB(166,180,210)))
dw_3.Modify("plde_codigo.BackGround.Color = " + String(RGB(166,180,210)))
dw_3.Modify("rfpe_numero.BackGround.Color = " + String(RGB(166,180,210)))

/*
Se actualiza tabla recfruprocee a objeto de bloquearla hasta que termine la grabación
del ingreso
*/
UPDATE dba.RECFRUPROCEE SET
 	rfpe_guides = 999
	WHERE rfpe_tarjas = 999
 	AND  rfpe_nrores = 999
 	AND  rfpe_tardef = 999;
		 
IF isnull(dw_3.Object.rfpe_numero[1]) OR dw_3.Object.rfpe_numero[1] = 0 THEN
	ll_nuevofolio=Buscanuevofolio(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]))
END IF

IF Long(ll_nuevofolio) = 0  THEN
	MessageBox("Advertencia","No Quedan Correlativos Disponibles, Proceda por Mantención.")
	Message.DoubleParm = -1
	Return 
END IF

IF isnull(dw_3.Object.rfpe_numero[1]) OR dw_3.Object.rfpe_numero[1] = 0 THEN
	dw_3.Object.rfpe_numero[1]	= ll_nuevofolio
	dw_3.SetItem(1, "rfpe_numero",ll_nuevofolio)
END IF	

istr_mant.argumento[2]	= String(ll_nuevofolio)

FOR li_fillas = 1 TO dw_1.RowCount()
	IF dw_1.Object.traspaso[li_fillas] = 1 THEN
		dw_1.Object.rfpe_numero[li_fillas]	= ll_nuevofolio
	END IF	
NEXT	

end event

type dw_3 from datawindow within w_maed_recfruprocee_gennueva
integer x = 37
integer y = 1412
integer width = 3177
integer height = 492
integer taborder = 90
boolean bringtotop = true
boolean titlebar = true
string title = "Recepción Nueva"
string dataobject = "dw_mant_recfruprocee_gennueva"
borderstyle borderstyle = stylelowered!
end type

