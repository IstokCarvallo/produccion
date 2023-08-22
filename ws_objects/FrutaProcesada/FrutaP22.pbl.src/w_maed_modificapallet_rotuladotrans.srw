$PBExportHeader$w_maed_modificapallet_rotuladotrans.srw
forward
global type w_maed_modificapallet_rotuladotrans from w_mant_encab_deta_csd
end type
type pb_recupera from picturebutton within w_maed_modificapallet_rotuladotrans
end type
type dw_3 from datawindow within w_maed_modificapallet_rotuladotrans
end type
end forward

shared variables

end variables

global type w_maed_modificapallet_rotuladotrans from w_mant_encab_deta_csd
boolean visible = false
integer width = 3849
integer height = 2056
string menuname = ""
event ue_imprimir ( )
pb_recupera pb_recupera
dw_3 dw_3
end type
global w_maed_modificapallet_rotuladotrans w_maed_modificapallet_rotuladotrans

type variables
Integer	ii_yaexiste, ii_estado, ii_ancestro, ii_Proceso, ii_bloquea, ii_bloqdaw
Long		il_NumProce
Boolean	ib_Existe

w_mant_deta_palletfruta_modifica_rotulatrans iw_mantencion

DataWindowChild	dw_especie, dw_etiqueta, dw_planta,&
                  dw_cliente,dw_condiciones,dw_emba
						
DataStore	ids_Palletencabhisto,ids_Palletfrutahisto

uo_variedadrotula			iuo_variedadrotula
end variables

forward prototypes
public function boolean existecondicion (integer as_valor)
public subroutine cuentacajas ()
public function boolean existevariedad (string ls_columna)
public function boolean existeplanta (integer as_valor)
public function boolean existevariecalibre (integer as_valor)
public function string embalajecliente (integer ai_cliente)
public function boolean existetipoembalaje (string as_codigo)
public subroutine buscavariedad ()
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso ()
public function boolean existepallet (string ls_columna)
public subroutine buscaembalaje ()
public function boolean existeembalaje (string ls_columna)
public function boolean existe_nrasda (string as_valor)
public function boolean controla_fecha (date fecha)
public subroutine buscaorigen ()
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean buscaingreso ()
public function boolean buscaembalaje (string ls_columna)
public subroutine bloqueaencabezado ()
public function boolean cliente_rotulado (integer cliente)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
//str_info	lstr_info

istr_info.titulo	= "RECEPCION DE PALLETS"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_palletencab_pallet" 

vinf.dw_1.GetChild("plde_codigo", dw_planta)
vinf.dw_1.GetChild("espe_codigo", dw_especie)
vinf.dw_1.GetChild("etiq_codigo", dw_etiqueta)
vinf.dw_1.GetChild("cond_codigo", dw_condiciones)

vinf.dw_1.SetTransObject(sqlca)
dw_planta.SetTransObject(sqlca)
dw_etiqueta.SetTransObject(sqlca)
dw_especie.SetTransObject(sqlca)
dw_condiciones.SetTransObject(sqlca)

dw_planta.Retrieve()

dw_especie.Retrieve()

dw_etiqueta.Retrieve()

dw_condiciones.Retrieve(integer(istr_mant.Argumento[10]))

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),Long(istr_mant.argumento[2]))		

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

public function boolean existecondicion (integer as_valor); integer	li_condicion
 String	ls_descondicion
 
 li_condicion	= as_valor
 
 SELECT cond_nombre 
    INTO :ls_descondicion  
    FROM dbo.condicion
   WHERE cond_codigo = : li_condicion   ;

IF sqlca.SQLCode = 0 THEN
	istr_mant.argumento[15]	=	ls_descondicion
ELSE
	istr_mant.argumento[15]	=	""
END IF
RETURN TRUE
end function

public subroutine cuentacajas ();Long	I
istr_mant.Argumento[11]	= string(dw_2.GetItemNumber(1, "paen_ccajas"))
For I=1 to dw_1.RowCount()
	istr_mant.argumento[11]     =	String(Long(istr_mant.argumento[11]) - dw_1.GetitemNumber(I,"pafr_ccajas"))
Next
IF	Long(istr_mant.argumento[11])<0 THEN  istr_mant.argumento[11]='0'

RETURN
end subroutine

public function boolean existevariedad (string ls_columna);Integer	li_cliente, li_especie, li_variedad
String	ls_nombre

li_cliente		=	Integer(istr_mant.argumento[1])
li_especie		=	dw_2.GetItemNumber(1, "espe_codigo")
li_variedad		=	Integer(ls_columna)

SELECT	vari_nombre
	INTO	:ls_nombre
	FROM	dbo.variedades
	WHERE	espe_codigo	= 	:li_especie
	AND	vari_codigo = 	:li_variedad ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Variedades")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Variedad no Existe. Ingrese Otra.", Exclamation!, Ok!)
	RETURN False
ELSE
	istr_mant.argumento[3]	= String(li_especie)
	istr_mant.argumento[4]	= String(li_variedad)
	istr_mant.argumento[5]	= ls_nombre
	dw_2.SetItem(1, "vari_nombre", ls_nombre)
	//dw_seca.Retrieve(gi_codexport,li_especie,li_variedad)
	RETURN True
END IF

RETURN False
end function

public function boolean existeplanta (integer as_valor);integer li_planta, li_cliente
string	ls_desplanta
 
li_planta	=	as_valor
li_cliente	= Integer(istr_mant.argumento[1])	
 
SELECT plde_nombre
INTO : ls_desplanta  
FROM  dbo.plantadesp
WHERE plde_codigo = :li_planta;

IF sqlca.SQLCode = 0 THEN
	istr_mant.argumento[14]	=	ls_desplanta
ELSE
	istr_mant.argumento[14]	=	""
END IF	

RETURN TRUE
end function

public function boolean existevariecalibre (integer as_valor);Integer	li_especie, li_variedad, li_cliente
String	ls_nombre, ls_secacod, ls_calibre
Long	   registros

li_especie					= integer(dw_2.Object.espe_codigo[1])
li_variedad					= as_valor
li_cliente					= integer(istr_mant.argumento[1])
ls_secacod					= dw_2.Object.seca_codigo[1]

SELECT	count(vaca_calibr)   
	INTO : registros
	FROM	dbo.variecalibre
	WHERE	espe_codigo		= : li_especie  and
			vari_codigo    = : li_variedad ;
			//and
			//seca_codigo		= : ls_secacod ;
IF (sqlca.SQLCode) = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Variecalibre")
	RETURN False
	
ELSEIF Registros < 1 THEN
	MessageBox("Atención","Código de Variedad-Calibre no Existe.", Exclamation!, Ok!)
	RETURN False
END IF
RETURN TRUE
end function

public function string embalajecliente (integer ai_cliente);String	ls_Embalaje

SELECT	Min(emba_codigo)
	INTO	:ls_Embalaje
	FROM	dbo.tipopallemba
	WHERE	clie_codigo	=	:ai_Cliente ;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tipos de Pallet por Embalaje")
END IF

RETURN ls_Embalaje
end function

public function boolean existetipoembalaje (string as_codigo);String	ls_embala
Integer	li_cliente, li_cancaj
Long		altura

li_cliente	= Integer(istr_mant.argumento[1])
ls_embala	= dw_2.Object.emba_codigo[1]

IF dw_2.Object.paen_tipopa[1]=2 THEN RETURN TRUE

SELECT	tpem_cancaj,tpem_altura
	INTO	:li_cancaj,:altura
	FROM	dbo.tipopallemba
	WHERE	clie_codigo	=	:li_Cliente
	AND	emba_codigo	=	:ls_embala
	AND	tpem_codigo	=	:as_codigo ;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla TipoPallemba")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Tipo de Embalaje no Existe para este Cliente.~rIngrese Otro.", Exclamation!, Ok!)
	RETURN False
ELSE
	istr_mant.Argumento[11]	= String(li_cancaj)
	dw_2.SetItem(1,"paen_ccajas",li_cancaj)
	dw_2.SetItem(1,"paen_altura",altura)
	Cuentacajas()
	RETURN True
END IF

RETURN False
end function

public subroutine buscavariedad ();Str_busqueda	lstr_busq

dw_2.Modify("buscavariedad.border = 0")
dw_2.Modify("buscavariedad.border = 5")

lstr_busq.argum[1]	=	String(dw_2.Object.espe_codigo[1])//istr_mant.Argumento[1]
lstr_busq.argum[2]	=	String(dw_2.GetItemNumber(1, "espe_codigo"))

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[4] <> '' THEN
	istr_mant.argumento[4]	=	lstr_busq.argum[3]
	istr_mant.argumento[5]	=	lstr_busq.argum[4]
	
	dw_2.setItem(1, "vari_codigo", Integer(lstr_busq.argum[3]))
	dw_2.setItem(1, "vari_nombre", lstr_busq.argum[4])
	dw_2.SetColumn("vari_codigo")
	dw_2.SetFocus()
END IF

dw_2.Modify("buscavariedad.border = 0")
dw_2.Modify("buscavariedad.border = 6")
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect		=	0
	dw_2.Object.paen_numero.Protect	=	0
	dw_2.Object.plde_codigo.Protect		=	0

	dw_2.Object.clie_codigo.Color 		= 0
	dw_2.Object.paen_numero.Color	= 0
	dw_2.Object.plde_codigo.Color		= 0
	
//	dw_2.Object.clie_codigo.BackGround.Color 		= RGB(255,255,255)
//	dw_2.Object.paen_numero.BackGround.Color	= RGB(255,255,255)
//	dw_2.Object.plde_codigo.BackGround.Color		= RGB(255,255,255)
	
	dw_2.SetColumn("clie_codigo")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clie_codigo.Protect		=	1
	dw_2.Object.paen_numero.Protect	=	1
	dw_2.Object.plde_codigo.Protect		=	1

//	dw_2.Object.clie_codigo.Color 		= RGB(255,255,255)
//	dw_2.Object.paen_numero.Color	= RGB(255,255,255)
//	dw_2.Object.plde_codigo.Color 	= RGB(255,255,255)
	
//	dw_2.Object.clie_codigo.BackGround.Color 		= 553648127
//	dw_2.Object.paen_numero.BackGround.Color	= 553648127
//	dw_2.Object.plde_codigo.BackGround.Color 	= 553648127
	
	IF dw_2.Object.paen_pmixto[1]	=	1 AND Integer(dw_2.Object.paen_pmixto.Protect)	=	1 THEN
		dw_2.Object.vari_codigo.Protect	=	1
		dw_2.Object.emba_codigo.Protect	=	1
		dw_2.Object.vari_codigo.Color 		= RGB(255,255,255)
		dw_2.Object.emba_codigo.Color	= RGB(255,255,255)
		
//		dw_2.Object.vari_codigo.BackGround.Color 		= 553648127
//		dw_2.Object.emba_codigo.BackGround.Color	= 553648127
	END IF
	IF ii_Ancestro = 1 THEN
		dw_2.Object.cond_codigo.Protect					=	1
		dw_2.Object.cond_codigo.Color 					= RGB(255,255,255)
//		dw_2.Object.cond_codigo.BackGround.Color	= 553648127
	END IF
END IF

end subroutine

public subroutine habilitaingreso ();Date		ld_fecha
Boolean	lb_estado = True

dw_2.AcceptText()

IF IsNull(dw_2.Object.paen_numero[1]) OR dw_2.Object.paen_numero[1] = 0 OR &
	IsNull(dw_2.Object.espe_codigo[1]) OR dw_2.Object.espe_codigo[1] = 0 OR &
	IsNull(dw_2.Object.vari_codigo[1]) OR dw_2.Object.vari_codigo[1] = 0 OR &
	IsNull(dw_2.Object.emba_codigo[1]) OR dw_2.Object.emba_codigo[1] = "" OR &
	IsNull(dw_2.Object.etiq_codigo[1]) OR &
	IsNull(dw_2.Object.cate_codigo[1]) OR dw_2.Object.cate_codigo[1] = 0 OR &
	IsNull(dw_2.Object.paen_fecemb[1]) OR dw_2.Object.paen_fecemb[1] = ld_fecha OR &
	IsNull(dw_2.Object.paen_cosecha[1])OR dw_2.Object.paen_cosecha[1] = ld_fecha OR &
	IsNull(dw_2.Object.stat_codigo[1]) OR dw_2.Object.stat_codigo[1] = 0 OR &
	IsNull(dw_2.Object.paen_ccajas[1]) OR dw_2.Object.paen_ccajas[1] = 0 OR &	
	IsNull(dw_2.Object.frio_codigo[1]) OR dw_2.Object.frio_codigo[1] = ""  THEN
	lb_estado = False
ELSEIF dw_2.Object.paen_tipopa[1] = 1 AND &
	IsNull(dw_2.Object.tpem_codigo[1]) OR dw_2.Object.tpem_codigo[1] = '' THEN
	lb_estado = False
END IF

//IF	IsNull(dw_2.Object.dest_codigo[1]) OR dw_2.Object.dest_codigo[1] = 0 THEN
//	lb_estado = False
//END IF

/* Valida que fecha ingresada este dentro del rango*/
IF lb_estado = TRUE THEN
	IF NOT controla_fecha(dw_2.Object.paen_fecemb[1]) THEN
		lb_estado = False
	ELSEIF NOT controla_fecha(dw_2.Object.paen_cosecha[1]) THEN
		lb_estado = False
	END IF
END IF


//pb_ins_det.Enabled = lb_estado
end subroutine

public function boolean existepallet (string ls_columna);Integer	li_cliente, li_planta, li_Movto
Long		ll_nropal, ll_pcopda, ll_cantid, ll_PFruta
Boolean	lb_retorno = False

li_cliente					=	Integer(dw_2.GetItemNumber(1, "clie_codigo"))
li_planta					=	Integer(dw_2.GetItemNumber(1, "plde_codigo"))

istr_mant.argumento[1]	=	string(dw_2.GetItemNumber(1, "clie_codigo"))
istr_mant.argumento[6]	=	string(dw_2.GetItemNumber(1, "plde_codigo"))

ll_nropal 					=	Long(ls_columna)
istr_mant.argumento[2]	=	String(ll_nropal)
li_Movto						= 0

SELECT	Count(*)
	INTO	:ll_cantid
	FROM	dbo.palletencab_trans
	WHERE	clie_codigo	= 	:li_cliente
	AND	paen_numero = 	:ll_nropal
	AND	plde_codigo	=	:li_planta;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
	lb_retorno = True
ELSEIF ll_cantid > 0 THEN
	IF ll_cantid > 0 THEN	
		SELECT 	Count(*)
				INTO	:ll_pcopda
				FROM	dbo.palletfruta_trans
				WHERE	clie_codigo	= 	:li_cliente
				AND	paen_numero = 	:ll_nropal
				AND	plde_codigo	=	:li_planta
				AND   pafr_secuen >  99999;
			
		IF sqlca.SQLCode = -1 THEN
			F_errorbasedatos(sqlca,"Lectura tabla palletfruta")
			lb_retorno = True
		ELSEIF ll_pcopda > 1 THEN	
					MessageBox("Atención","El Numero de Pallet Ingresado es un Ingreso Caja a Caja. Ingrese Otro.", Exclamation!, Ok!)
			lb_retorno = True		
		END IF	
	END IF
		
		SELECT 	Count(*)
		INTO	:ll_pcopda
		FROM	dbo.palletencab_trans
		WHERE	clie_codigo	= 	:li_cliente
		AND	paen_numero = 	:ll_nropal
		AND	plde_codigo	=	:li_planta
		AND	paen_pcopda = 2;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla palletencab_trans")
		lb_retorno = True
	ELSEIF ll_pcopda > 0 THEN	
			MessageBox("Atención","El Numero de Pallet Ingresado es un Ingreso Caja a Caja. Ingrese Otro.", Exclamation!, Ok!)
			lb_retorno = True		
	ELSE
	   SELECT	Count(*) 
		  INTO	:ll_PFruta
		  FROM	dbo.palletfruta_trans
		  WHERE 	clie_codigo	= 	:li_cliente
		  AND	 	paen_numero	= 	:ll_nropal
		  AND	 	plde_codigo	=	:li_planta;
		
	   IF sqlca.SQLCode = -1 THEN
			F_errorbasedatos(sqlca,"Lectura tabla PalletFruta")
			lb_retorno = True
	   ELSEIF ll_PFruta > 0 THEN		
			IF ii_ancestro = 1 THEN
				IF Not buscaingreso() THEN
					li_Movto	= 1				
					lb_retorno = True						
				END IF
			END IF
			IF ii_ancestro = 0 OR li_Movto = 0 THEN
				This.TriggerEvent("ue_recuperadatos")
				
				bloqueaencabezado()
								
				IF ii_ancestro = 1  THEN
					istr_mant.argumento[3] = String(dw_2.Object.espe_codigo[1])
					istr_mant.argumento[4] = String(dw_2.Object.vari_codigo[1])
					istr_mant.argumento[5] = dw_2.Object.vari_nombre[1]
					istr_mant.argumento[6] = String(dw_2.Object.plde_codigo[1])
					istr_mant.argumento[7] = dw_2.Object.emba_codigo[1]
					istr_mant.argumento[8] = dw_2.Object.emba_nombre[1]
					istr_mant.argumento[9] = String(dw_2.Object.etiq_codigo[1])
					istr_mant.argumento[10] = String(dw_2.Object.cond_codigo[1])
					istr_mant.argumento[12] = ""
				
					IF ExisteEmbalaje(istr_mant.argumento[7]) THEN
						dw_emba.Retrieve(li_cliente,istr_mant.argumento[7])
					END IF
					
					//ExisteTipoEmbalaje(String(dw_2.Object.tpem_codigo[1]))
					ExistePlanta(dw_2.Object.plde_codigo[1])
					//ExistePlanta(gi_codplanta)
					Existecondicion(dw_2.Object.cond_codigo[1])		
					lb_retorno = False		
				END IF
			END IF				
	   ELSE		
			MessageBox("Atención","El Numero de Pallet No tiene Detalle. Ingrese Otro.", Exclamation!, Ok!)
			lb_retorno = True
 	   END IF		
	END IF
ELSE
	MessageBox("Atención","El Numero de Pallet Ingresado No Existe. Ingrese Otro.", Exclamation!, Ok!)
	lb_retorno = True
END IF

RETURN lb_retorno
end function

public subroutine buscaembalaje ();Str_busqueda		lstr_busq

dw_2.Modify("buscaembalaje.border = 0")
dw_2.Modify("buscaembalaje.border = 5")

lstr_busq.Argum[1]	=	istr_mant.Argumento[1]

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.Argum[2] <> "" THEN
	istr_mant.Argumento[7]	=	lstr_busq.Argum[2]
	istr_mant.Argumento[8]	=	lstr_busq.Argum[3]
	
	dw_2.setItem(1, "emba_codigo", lstr_busq.Argum[2])
	dw_2.setItem(1, "emba_nombre", lstr_busq.Argum[3])
	dw_2.SetItem(1, "tiem_codigo", Integer(lstr_busq.Argum[5]))
	
	dw_emba.Retrieve(Integer(istr_mant.Argumento[1]), lstr_busq.Argum[2])
ELSE
	dw_2.SetColumn("emba_codigo")
	dw_2.SetFocus()
END IF

dw_2.Modify("buscaembalaje.border = 0")
dw_2.Modify("buscaembalaje.border = 6")
end subroutine

public function boolean existeembalaje (string ls_columna);String	ls_codigo, ls_nombre
Integer	li_cliente, li_tipoen

li_cliente	= Integer(istr_mant.argumento[1])
ls_codigo	= ls_columna

SELECT	emb.emba_nombre, env.enva_tipoen
	INTO 	:ls_nombre, :li_tipoen
	FROM	dbo.embalajesprod as emb, dbo.envases as env
	WHERE	emb.clie_codigo	= :li_cliente
	AND	emb.emba_codigo	= :ls_codigo
	AND	env.enva_tipoen	= emb.enva_tipoen
	AND	env.enva_codigo	= emb.enva_codigo;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Emabalajes")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Embalaje no Existe para este Cliente. Ingrese Otro.", Exclamation!, Ok!)
	RETURN False
ELSE
	istr_mant.argumento[7]	=	ls_codigo
	istr_mant.argumento[8]	=	ls_nombre
	dw_2.SetItem(1, "emba_nombre", ls_nombre)
	dw_2.SetItem(1, "tiem_codigo", li_tipoen)
	RETURN True
END IF

end function

public function boolean existe_nrasda (string as_valor);Long	registros
  
  SELECT Count(*)  
    INTO :registros
    FROM dbo.Palletencab
   WHERE paen_nrasda = :as_valor;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletfruta")
	RETURN TRUE
ELSEIF registros > 0 THEN
	MessageBox("Atención","Nro de ASDA ya fue ingresado en otro Registro", Exclamation!, Ok!)
	RETURN TRUE
END IF

RETURN FALSE
end function

public function boolean controla_fecha (date fecha);IF fecha >= gd_TempoInicio AND fecha <= gd_TempoTermin THEN
	RETURN True
ElSE
	RETURN False
END IF
end function

public subroutine buscaorigen ();Integer	li_Cliente, li_Planta, li_Cuenta
Long		ll_Numero

li_Cliente	=	dw_2.Object.clie_codigo[1]
li_Planta	=	dw_2.Object.plde_codigo[1]
ll_Numero	=	dw_2.Object.paen_numero[1]

SELECT Count(*) INTO :li_Cuenta
FROM dbo.recfruproced
WHERE clie_codigo = :li_Cliente
AND   plde_codigo = :li_Planta
AND   paen_numero = :ll_Numero;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla recfruproced")
   istr_mant.argumento[20]	=	'0'
ELSEIF sqlca.SQLCode = 100 THEN
		 istr_mant.argumento[20]	=	'0'	 
	ELSE
		 istr_mant.argumento[20]	=	'1'
END IF


RETURN

end subroutine

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

IF Not dw_2.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
//	IF ids_Palletfrutahisto.Update(True, False) = 1 THEN
//		IF ids_Palletencabhisto.Update(True, False) = 1 THEN
//			Commit;
//			
//			IF sqlca.SQLCode <> 0 THEN
//				F_ErrorBaseDatos(sqlca, This.Title)
//				
//				RollBack;
//			ELSE
//				lb_Retorno	=	True
//				
//				ids_Palletfrutahisto.ResetUpdate()
//				ids_Palletencabhisto.ResetUpdate()							
//			END IF
//		ELSE
//			F_ErrorBaseDatos(sqlca, This.Title)
//			
//			RollBack;
//		END IF
//	ELSE
//		F_ErrorBaseDatos(sqlca, This.Title)
//		
//		RollBack;
//	END IF	
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
	//IF GrabaHistoria() THEN 
		IF dw_2.Update(True, False) = 1 THEN
			IF dw_1.Update(True, False) = 1 THEN
		//		IF dw_histoEncab.Update(True, False) = 1 THEN
			//		IF dw_histoFruta.Update(True, False) = 1 THEN
						Commit;
						 
						IF sqlca.SQLCode <> 0 THEN
							F_ErrorBaseDatos(sqlca, This.Title)
							
							RollBack;
						ELSE
							lb_Retorno	=	True
							
							dw_1.ResetUpdate()
							dw_2.ResetUpdate()
//							dw_histoEncab.ResetUpdate()
//							dw_histoFruta.ResetUpdate()							
						END IF
//					ELSE
//						F_ErrorBaseDatos(sqlca, This.Title)
//						
//						RollBack;
//					END IF
//				ELSE
//					F_ErrorBaseDatos(sqlca, This.Title)
//					
//					RollBack;
//				END IF
			ELSE
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
		END IF
	//ELSE	
	//	F_ErrorBaseDatos(sqlca, This.Title)
	//END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public function boolean buscaingreso ();//Integer	li_Cliente, li_Planta, li_Cuenta, li_Cuenta2,  &
//			li_Cuenta1, li_TipoEn, li_Origen, li_Cuenta6, li_estado
//Long		ll_Numero, ll_Folio, ll_rfpe_numero, ll_Recepcion, ll_GuiaDespa
//Date		ld_Fecha
//String	ls_vacia
//
//li_Cliente	=	dw_2.Object.clie_codigo[1]
//li_Planta	=	dw_2.Object.plde_codigo[1]
//ll_Numero	=	Long(istr_mant.argumento[2]) //dw_2.Object.paen_numero[1]
//ls_vacia		=	istr_mant.argumento[21]
//
////cambio solicitado por rancagua para modificar el tipo de frio y tipo pallet
////14/05/2008
//
//SELECT 	Count() INTO :li_Cuenta
//  FROM 	dbo.recfruproced_trans
//  WHERE 	clie_codigo = :li_Cliente
//  AND   	plde_codigo = :li_Planta
//  AND   	paen_numero = :ll_Numero;
//  
//IF li_Cuenta = 0 THEN
//	li_Cuenta6 = 1
//END IF	
//
//li_Cuenta = 1
//
//IF sqlca.SQLCode = -1 THEN
//	F_errorbasedatos(sqlca,"Lectura Tabla Recfruproced")
//   RETURN False
//ELSEIF li_Cuenta = 0 THEN
//	MessageBox("Atención","Número de Pallet No Generado en Recepción. Ingrese Otro.", Exclamation!, Ok!)
//	RETURN False	 
//ELSEIF li_Cuenta > 0 THEN
//	 SELECT  rfpe_numero 
//		INTO  :ll_rfpe_numero
//		FROM  dbo.recfruproced_trans
//		WHERE clie_codigo = :li_Cliente
//		AND   plde_codigo = :li_Planta
//		AND   paen_numero = :ll_Numero;
//		
//	 SELECT	spcr_estado
//		INTO 	:li_estado
//		FROM 	dbo.spro_controlrecepciones
//		WHERE plde_codigo = :li_Planta
//		AND	clie_codigo = :li_Cliente
//		AND   rfpe_numero = :ll_rfpe_numero;
//
//	 SELECT rfpe_numero, rfpe_guides
//	 	INTO :ll_Recepcion, :ll_GuiaDespa
//		FROM  dbo.recfruprocee_trans
//		WHERE clie_codigo = :li_Cliente
//		AND   plde_codigo = :li_Planta
//		AND   rfpe_numero = :ll_rfpe_numero;	 
//	 
//	IF ll_Recepcion=ll_GuiaDespa THEN
//	 SELECT Count() INTO :li_Cuenta2
//		FROM dbo.Inspecpaldet_trans
//		WHERE clie_codigo = :li_Cliente
//		AND   plde_codigo = :li_Planta
//		AND   paen_numero = :ll_Numero;
//	END IF
//	
//	 IF IsNull(li_Cuenta1) THEN li_Cuenta1 = 0
//	 IF IsNull(li_Cuenta2) THEN li_Cuenta2 = 0
//	 IF IsNull(li_Cuenta6) THEN li_Cuenta6 = 0
//
//	 IF (li_Cuenta1 + li_Cuenta2 + li_Cuenta6 + li_estado) > 0 THEN
//			IF li_Cuenta6 = 0 THEN
//				MessageBox("Atención","Número de Pallet de Recepción Transitorio ya esta Definitivo. Ingrese Otro.", Exclamation!, Ok!)
//				//RETURN False
//			END IF	
//			ii_bloquea = 1  
//	 ELSE
//		ii_bloquea = 0
//		RETURN True
//	 END IF
//
//ELSE
//	 MessageBox("Atención","Número de Pallet No Generado en Recepción. Ingrese Otro.", Exclamation!, Ok!)			
//	 RETURN False
//END IF	

RETURN True
end function

public function boolean buscaembalaje (string ls_columna);Integer	li_cliente, li_planta, li_Movto
Long		ll_nropal, ll_pcopda, ll_cantid, ll_PFruta
Boolean	lb_retorno = False

li_cliente					=	Integer(dw_2.GetItemNumber(1, "clie_codigo"))
li_planta					=	Integer(dw_2.GetItemNumber(1, "plde_codigo"))
ll_nropal 					=	Long(ls_columna)
istr_mant.argumento[2]	=	String(ll_nropal)
li_Movto						= 0

SELECT	Count(*)
	INTO	:ll_cantid
	FROM	dbo.palletencab
	WHERE	clie_codigo	= 	:li_cliente
	AND	paen_numero = 	:ll_nropal
	AND	plde_codigo	=	:li_planta;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
	lb_retorno = True
ELSEIF ll_cantid > 0 THEN
	SELECT 	Count(*)
		INTO	:ll_pcopda
		FROM	dbo.spro_palletencab
		WHERE	clie_codigo	= 	:li_cliente
		AND	paen_numero = 	:ll_nropal
		AND	plde_codigo	=	:li_planta;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Spro_PalletEncab")
		lb_retorno = True
	ELSEIF ll_pcopda > 0 THEN	
			MessageBox("Atención","El Numero de Pallet Ingresado es un Ingreso Caja a Caja. Ingrese Otro.", Exclamation!, Ok!)
			lb_retorno = True		
	ELSE
	   SELECT	Count(*) 
		  INTO	:ll_PFruta
		  FROM	dbo.palletfruta
		  WHERE 	clie_codigo	= 	:li_cliente
		  AND	 	paen_numero	= 	:ll_nropal
		  AND	 	plde_codigo	=	:li_planta;
		
	   IF sqlca.SQLCode = -1 THEN
			F_errorbasedatos(sqlca,"Lectura tabla PalletFruta")
			lb_retorno = True
	   ELSEIF ll_PFruta > 0 THEN		
				IF ii_ancestro = 1 THEN
					IF Not buscaingreso() THEN
						li_Movto	= 1				
						lb_retorno = True						
					END IF
				END IF
				IF ii_ancestro = 0 OR li_Movto = 0 THEN
					This.TriggerEvent("ue_recuperadatos")
					
					   istr_mant.argumento[3] = String(dw_2.Object.espe_codigo[1])
						istr_mant.argumento[4] = String(dw_2.Object.vari_codigo[1])
						istr_mant.argumento[5] = dw_2.Object.vari_nombre[1]
						istr_mant.argumento[6] = String(dw_2.Object.plde_codigo[1])
						istr_mant.argumento[7] = dw_2.Object.emba_codigo[1]
						istr_mant.argumento[8] = dw_2.Object.emba_nombre[1]
						istr_mant.argumento[9] = String(dw_2.Object.etiq_codigo[1])
						istr_mant.argumento[10] = String(dw_2.Object.cond_codigo[1])
						istr_mant.argumento[12] = ""
					
						IF ExisteEmbalaje(istr_mant.argumento[7]) THEN
							dw_emba.Retrieve(li_cliente,istr_mant.argumento[7])
						END IF
						
						//ExisteTipoEmbalaje(String(dw_2.Object.tpem_codigo[1]))
						ExistePlanta(dw_2.Object.plde_codigo[1])
						//ExistePlanta(gi_codplanta)
						Existecondicion(dw_2.Object.cond_codigo[1])		
						lb_retorno = False		
				END IF				
	   ELSE		
			MessageBox("Atención","El Numero de Pallet No tiene Detalle. Ingrese Otro.", Exclamation!, Ok!)
			lb_retorno = True
 	   END IF		
	END IF
ELSE
	MessageBox("Atención","El Numero de Pallet Ingresado No Existe. Ingrese Otro.", Exclamation!, Ok!)
	lb_retorno = True
END IF

RETURN lb_retorno
end function

public subroutine bloqueaencabezado ();IF ii_bloquea = 0 THEN
	dw_2.Object.clie_codigo.Protect		=	0   
	dw_2.Object.paen_numero.Protect	=	0   
	dw_2.Object.paen_tipopa.Protect	=	0   
	dw_2.Object.espe_codigo.Protect	=	0  
	dw_2.Object.vari_codigo.Protect	=	0
	dw_2.Object.emba_codigo.Protect	=	0 
	dw_2.Object.cate_codigo.Protect	=	0 
	dw_2.Object.etiq_codigo.Protect	=	0 
	dw_2.Object.stat_codigo.Protect	=	0 
	dw_2.Object.trat_codigo.Protect	=	0 
	dw_2.Object.frio_codigo.Protect	=	0 
	dw_2.Object.cond_codigo.Protect	=	1  //dw_2.Object.cond_codigo.Protect	=	0 
	dw_2.Object.plde_codigo.Protect	=	0 
	dw_2.Object.paen_cosecha.Protect	=	0 
	dw_2.Object.paen_altura.Protect	=	0
	dw_2.Object.tpem_codigo.Protect	=	0
	dw_2.Object.paen_ccajas.Protect	=	0
	dw_2.Object.paen_concal.Protect	=	1	//dw_2.Object.paen_concal.Protect	=	0
	dw_2.Object.paen_inspec.Protect	=	1  //dw_2.Object.paen_inspec.Protect	=	0
	dw_2.Object.dest_codigo.Protect	=	0
	dw_2.Object.paen_pexpor.Protect	=	0
	dw_2.Object.paen_pmixto.Protect	=	0
	dw_2.Object.paen_nrasda.Protect	=	0
	dw_2.Object.copa_codigo.Protect	=	0
	dw_2.Object.paen_fecemb.Protect	=	0
	dw_2.Object.paen_cosecha.Protect	=	0
	dw_2.Object.paen_ccajas.Protect	=	0
	
	dw_2.Object.clie_codigo.Color 		= 0
	dw_2.Object.paen_numero.Color 	= 0
	dw_2.Object.paen_tipopa.Color 	= 0
	dw_2.Object.espe_codigo.Color 	= 0
	dw_2.Object.vari_codigo.Color 		= 0
	dw_2.Object.emba_codigo.Color 	= 0
	dw_2.Object.cate_codigo.Color 	= 0
	dw_2.Object.etiq_codigo.Color 		= 0
	dw_2.Object.stat_codigo.Color 		= 0
	dw_2.Object.trat_codigo.Color 		= 0
	dw_2.Object.frio_codigo.Color 		= 0
	dw_2.Object.plde_codigo.Color 	= 0
	dw_2.Object.paen_cosecha.Color 	= 0
	dw_2.Object.paen_altura.Color 	= 0
	dw_2.Object.tpem_codigo.Color 	= 0
	dw_2.Object.paen_ccajas.Color 	= 0
	dw_2.Object.dest_codigo.Color 	= 0
	dw_2.Object.paen_pexpor.Color 	= 0
	dw_2.Object.paen_pmixto.Color 	= 0
	dw_2.Object.paen_nrasda.Color 	= 0
	dw_2.Object.copa_codigo.Color 	= 0
	dw_2.Object.paen_fecemb.Color 	= 0
	dw_2.Object.paen_cosecha.Color 	= 0
	dw_2.Object.paen_ccajas.Color 	= 0
	
	dw_2.Object.clie_codigo.BackGround.Color 		= RGB(255,255,255)
	dw_2.Object.paen_numero.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.paen_tipopa.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.espe_codigo.BackGround.Color 	= RGB(255,255,255) 
	dw_2.Object.vari_codigo.BackGround.Color 		= RGB(255,255,255)
	dw_2.Object.emba_codigo.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.cate_codigo.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.etiq_codigo.BackGround.Color 		= RGB(255,255,255) 
	dw_2.Object.stat_codigo.BackGround.Color 		= RGB(255,255,255)
	dw_2.Object.trat_codigo.BackGround.Color 		= RGB(255,255,255) 
	dw_2.Object.frio_codigo.BackGround.Color 		= RGB(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color 	= RGB(255,255,255) 
	dw_2.Object.paen_cosecha.BackGround.Color 	= RGB(255,255,255) 
	dw_2.Object.paen_altura.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.tpem_codigo.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.paen_ccajas.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.dest_codigo.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.paen_pexpor.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.paen_pmixto.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.paen_nrasda.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.copa_codigo.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.paen_fecemb.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.paen_cosecha.BackGround.Color 	= RGB(255,255,255)
	dw_2.Object.paen_ccajas.BackGround.Color 	= RGB(255,255,255)
	ii_bloqdaw = 0

	dw_2.Object.cond_codigo.Color = RGB(255,255,255)
	dw_2.Object.paen_concal.Color = RGB(255,255,255)
	dw_2.Object.paen_inspec.Color = RGB(255,255,255)
	
	dw_2.Object.cond_codigo.BackGround.Color = 553648127
	dw_2.Object.paen_concal.BackGround.Color = 553648127
	dw_2.Object.paen_inspec.BackGround.Color = 553648127

		
	dw_2.Enabled = True	 
ELSE
	
	dw_2.Object.clie_codigo.Protect	=	1   
	dw_2.Object.paen_numero.Protect	=	1   
	dw_2.Object.paen_tipopa.Protect	=	1   
	dw_2.Object.espe_codigo.Protect	=	1  
	dw_2.Object.vari_codigo.Protect	=	1
	dw_2.Object.emba_codigo.Protect	=	1
	dw_2.Object.cate_codigo.Protect	=	1 
	dw_2.Object.etiq_codigo.Protect	=	1 
	dw_2.Object.stat_codigo.Protect	=	1 
	dw_2.Object.trat_codigo.Protect	=	1 
	dw_2.Object.frio_codigo.Protect	=	1 
	dw_2.Object.cond_codigo.Protect	=	1 
	dw_2.Object.plde_codigo.Protect	=	1 
	dw_2.Object.paen_cosecha.Protect	=	1 
	dw_2.Object.paen_altura.Protect	=	1
	dw_2.Object.tpem_codigo.Protect	=	1
	dw_2.Object.paen_ccajas.Protect	=	1
	dw_2.Object.paen_concal.Protect	=	1
	dw_2.Object.paen_inspec.Protect	=	1
	dw_2.Object.dest_codigo.Protect	=	1
	dw_2.Object.paen_pexpor.Protect	=	1
	dw_2.Object.paen_pmixto.Protect	=	1
	dw_2.Object.paen_nrasda.Protect	=	1
	dw_2.Object.copa_codigo.Protect	=	1
	dw_2.Object.paen_fecemb.Protect	=	1
	dw_2.Object.paen_cosecha.Protect	=	1
	dw_2.Object.paen_ccajas.Protect	=	1
	
	dw_2.Object.clie_codigo.Color 		= RGB(255,255,255)
	dw_2.Object.paen_numero.Color 	= RGB(255,255,255)
	dw_2.Object.paen_tipopa.Color 	= RGB(255,255,255)
	dw_2.Object.espe_codigo.Color 	= RGB(255,255,255)
	dw_2.Object.vari_codigo.Color 		= RGB(255,255,255)
	dw_2.Object.emba_codigo.Color 	= RGB(255,255,255)
	dw_2.Object.cate_codigo.Color 	= RGB(255,255,255)
	dw_2.Object.etiq_codigo.Color 		= RGB(255,255,255)
	dw_2.Object.stat_codigo.Color 		= RGB(255,255,255)
	dw_2.Object.trat_codigo.Color 		= RGB(255,255,255)
	dw_2.Object.frio_codigo.Color 		= RGB(255,255,255)
	dw_2.Object.cond_codigo.Color 	= RGB(255,255,255)
	dw_2.Object.plde_codigo.Color 	= RGB(255,255,255)
	dw_2.Object.paen_cosecha.Color 	= RGB(255,255,255)
	dw_2.Object.paen_altura.Color 	= RGB(255,255,255)
	dw_2.Object.tpem_codigo.Color 	= RGB(255,255,255)
	dw_2.Object.paen_ccajas.Color 	= RGB(255,255,255)
	dw_2.Object.paen_inspec.Color 	= RGB(255,255,255)
	dw_2.Object.dest_codigo.Color 	= RGB(255,255,255)
	dw_2.Object.paen_pexpor.Color 	= RGB(255,255,255)
	dw_2.Object.paen_pmixto.Color 	= RGB(255,255,255)
	dw_2.Object.paen_nrasda.Color 	= RGB(255,255,255)
	dw_2.Object.copa_codigo.Color 	= RGB(255,255,255)
	dw_2.Object.paen_fecemb.Color 	= RGB(255,255,255)
	dw_2.Object.paen_cosecha.Color 	= RGB(255,255,255)
	dw_2.Object.paen_ccajas.Color 	= RGB(255,255,255)
	
	
	dw_2.Object.clie_codigo.BackGround.Color 		= 553648127
	dw_2.Object.paen_numero.BackGround.Color 	= 553648127
	dw_2.Object.paen_tipopa.BackGround.Color 	= 553648127
	dw_2.Object.espe_codigo.BackGround.Color 	= 553648127
	dw_2.Object.vari_codigo.BackGround.Color 		= 553648127
	dw_2.Object.emba_codigo.BackGround.Color 	= 553648127
	dw_2.Object.cate_codigo.BackGround.Color 	= 553648127
	dw_2.Object.etiq_codigo.BackGround.Color 		= 553648127
	dw_2.Object.stat_codigo.BackGround.Color 		= 553648127
	dw_2.Object.trat_codigo.BackGround.Color 		= 553648127
	dw_2.Object.frio_codigo.BackGround.Color 		= 553648127
	dw_2.Object.cond_codigo.BackGround.Color 	= 553648127
	dw_2.Object.plde_codigo.BackGround.Color 	= 553648127
	dw_2.Object.paen_cosecha.BackGround.Color 	= 553648127
	dw_2.Object.paen_altura.BackGround.Color 	= 553648127
	dw_2.Object.tpem_codigo.BackGround.Color 	= 553648127
	dw_2.Object.paen_ccajas.BackGround.Color 	= 553648127
	dw_2.Object.paen_inspec.BackGround.Color 	= 553648127
	dw_2.Object.dest_codigo.BackGround.Color 	= 553648127
	dw_2.Object.paen_pexpor.BackGround.Color 	= 553648127
	dw_2.Object.paen_pmixto.BackGround.Color 	= 553648127
	dw_2.Object.paen_nrasda.BackGround.Color 	= 553648127
	dw_2.Object.copa_codigo.BackGround.Color 	= 553648127
	dw_2.Object.paen_fecemb.BackGround.Color 	= 553648127
	dw_2.Object.paen_cosecha.BackGround.Color 	= 553648127
	dw_2.Object.paen_ccajas.BackGround.Color 	= 553648127
	
	ii_bloqdaw = 1
	dw_2.Enabled = False
END IF	
end subroutine

public function boolean cliente_rotulado (integer cliente);Boolean 	lb_retorno
String	ls_clierotulado

SELECT isnull(clie_rotula,'') 
INTO :ls_clierotulado
FROM dbo.clientesprod 
WHERE clie_codigo = :cliente;
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla clientesprod")
	lb_retorno = True
ELSEIF String(ls_clierotulado) <> ''  THEN
	dw_2.Object.paen_nrasda[1] = String(ls_clierotulado)
	lb_retorno = False	
ELSE	
	dw_2.Object.paen_nrasda[1] = String(cliente)
	lb_retorno = False
END IF	
	   
RETURN lb_retorno

end function

event open;/*
	Argumentos :	[1]	=	Código de Exportador
						[2]	=	
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "paen_numero", Long(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "espe_codigo", Integer(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "vari_codigo", Integer(istr_mant.argumento[4]))
	dw_1.SetItem(il_fila, "variedades_vari_nombre", istr_mant.argumento[5])
	dw_1.SetItem(il_fila, "emba_codigo", istr_mant.argumento[7])
	dw_1.SetItem(il_fila, "embalajes_emba_nombre", istr_mant.argumento[8])
	dw_1.SetItem(il_fila, "cond_codigo", Integer(istr_mant.argumento[10]))
	dw_1.SetItem(il_fila, "etiq_codigo", Integer(istr_mant.argumento[9]))
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[6]))
	dw_1.SetItem(il_fila, "pafr_ccajas", il_ccajas)
	dw_1.SetItem(il_fila, "cond_nombre", istr_mant.argumento[13])				
	dw_1.SetItem(il_fila, "plde_nombre", istr_mant.argumento[14])					
	dw_1.SetItem(il_fila, "descrip_condicion", istr_mant.argumento[15])					
*/
String				ls_movto

x				= 0
y				= 0
This.Height	= 2500
im_menu		= m_principal

IF Not f_validafechatempo(today()) THEN
   Messagebox('Atención','Aclare Fecha Actual Temporada con Informática')
END IF

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				=	dw_1
istr_mant.dw2				=	dw_3

istr_mant.solo_consulta	=	False
istr_mant.argumento[14]	=	''
istr_mant.argumento[16]	=	''

pb_nuevo.PostEvent(Clicked!)

ls_movto	= Message.StringParm

ii_Proceso		=	0
ii_ancestro		= 	0
IF ls_movto = "" THEN
	ii_ancestro		= 	1
END IF

dw_2.GetChild("clie_codigo", dw_cliente)
dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("espe_codigo", dw_especie)
dw_2.GetChild("etiq_codigo", dw_etiqueta)
dw_2.GetChild("tpem_codigo", dw_emba)

dw_cliente.SetTransObject(sqlca)
dw_planta.SetTransObject(sqlca)
dw_especie.SetTransObject(sqlca)
dw_etiqueta.SetTransObject(sqlca)
dw_emba.SetTransObject(sqlca)

dw_cliente.Retrieve()
dw_planta.Retrieve(1)
dw_especie.Retrieve()
dw_etiqueta.Retrieve()
dw_emba.Retrieve(gi_codexport, gs_CodEmbalaje)

istr_mant.argumento[1]	=	String(gi_CodExport)
istr_mant.argumento[6]	=	String(gi_CodPlanta)
istr_mant.argumento[7]	=	gs_CodEmbalaje
istr_mant.argumento[22]	=	'1'		//	Pallet Exportación
istr_mant.argumento[23]	=	'0'		//	Pallet Mixto

dw_2.Object.paen_pmixto.Protect	=	1

IF ls_movto = "" THEN
	GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Mantención de Pallet Histórico", 1)
END IF

iuo_variedadrotula			=	CREATE	uo_variedadrotula

buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"


end event

event ue_borra_detalle;IF dw_1.rowcount() < 1 THEN
	dw_1.SetFocus()
	RETURN
END IF

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF dw_1.RowCount() = 0 THEN 
		HabilitaEncab(True)
		pb_eli_det.Enabled = False
	ELSEIF dw_1.RowCount() > 1 THEN	
		pb_eli_det.Enabled = True
	END IF
END IF

IF dw_1.RowCount() > 1 THEN	
	pb_eli_det.Enabled = True
ELSE	
	pb_eli_det.Enabled = False
END IF
istr_mant.borra	 = False
end event

event ue_nuevo_detalle;istr_mant.borra	= False
istr_mant.agrega	= True
cuentacajas()
istr_mant.argumento[28]=String(dw_2.object.paen_fecemb[1])
istr_mant.argumento[39]=String(Today())

buscaorigen()

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, ll_fila_fh, ll_fila_eh
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]), &
							Integer(istr_mant.argumento[6]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos 1.", &
										Information!, RetryCancel!)
	ELSE
		istr_mant.Argumento[22]	=	String(dw_2.Object.paen_pexpor[1])
		istr_mant.Argumento[23]	=	String(dw_2.Object.paen_pmixto[1])
		istr_mant.argumento[40] 	=  String(dw_2.Object.paen_fecemb[1])
		ii_estado							=	dw_2.Object.paen_estado[1]
		
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), &
												 Long(istr_mant.argumento[2]), &
												 Integer(istr_mant.argumento[6]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos 2.", &
												Information!, RetryCancel!)
			ELSE
				pb_imprimir.Enabled	= True
				IF ii_estado=2 OR ii_estado=3 THEN
					istr_mant.solo_consulta	=	True	
				ELSE
					istr_mant.solo_consulta	=	False
					IF ii_bloqdaw = 0 THEN
						pb_eliminar.Enabled		= True
						pb_grabar.Enabled			= True				
						pb_ins_det.Enabled		= False
					ELSE	
						pb_eliminar.Enabled		= False
						pb_grabar.Enabled			= False
						pb_ins_det.Enabled		= False
					END IF	
				END IF
				IF ll_fila_d > 0 THEN
					IF  ii_estado=2 OR ii_estado=3 THEN
						pb_eli_det.Enabled = False
					ELSE						
						dw_1.SetRow(1)
						dw_1.SelectRow(1,True)
						dw_1.SetFocus()
						HabilitaEncab(False)
					END IF
				ELSE
					IF  ii_estado<>2 AND ii_estado<>3 THEN pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_maed_modificapallet_rotuladotrans.create
int iCurrent
call super::create
this.pb_recupera=create pb_recupera
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_recupera
this.Control[iCurrent+2]=this.dw_3
end on

on w_maed_modificapallet_rotuladotrans.destroy
call super::destroy
destroy(this.pb_recupera)
destroy(this.dw_3)
end on

event ue_nuevo;call super::ue_nuevo;HabilitaEncab(True)

dw_2.SetItem(1, "clie_codigo", Integer(istr_mant.Argumento[1]))
dw_2.SetItem(1, "plde_codigo", Integer(istr_mant.Argumento[6]))
dw_2.SetItem(1, "espe_codigo", gi_CodEspecie)

istr_mant.argumento[3]	=	String(gi_CodEspecie)
istr_mant.argumento[9]	=	"1"
istr_mant.argumento[10]	=	"0"

dw_2.Object.vari_codigo.Protect	=	0
dw_2.Object.emba_codigo.Protect	=	0
dw_2.Object.vari_codigo.BackGround.Color = RGB(255,255,255)
dw_2.Object.emba_codigo.BackGround.Color = RGB(255,255,255)

end event

event ue_seleccion;call super::ue_seleccion;istr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1]) 
istr_busq.argum[2]	=	""
istr_busq.argum[5]	=  ""

OpenWithParm(w_busc_palletencab, istr_busq)

istr_busq = Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	istr_mant.argumento[1] = istr_busq.argum[1]
	istr_mant.argumento[2] = istr_busq.argum[2]
	istr_mant.argumento[6] = istr_busq.argum[7]
	
	This.TriggerEvent("ue_recuperadatos")
	ExistePallet(string(dw_2.Object.paen_numero[1]))
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_fila, ll_numero, ll_fila_P, ll_CajasDet, ll_null
Integer	li_Secuencia, li_cliente, li_planta, li_paen_mixto, li_respue

ll_CajasDet	=	 0

SetNull(ll_null)

iuo_VariedadRotula.existe(dw_2.Object.espe_codigo[1],dw_2.Object.vari_codigo[1], True, sqlca) 

dw_2.Object.paen_varrot[1] = iuo_variedadrotula.Varirotula
//dw_2.Object.paen_nrasda[1] = Istr_mant.argumento[32]

li_cliente	=	dw_2.Object.clie_codigo[1]
li_planta	=	dw_2.Object.plde_codigo[1]
ll_numero	=	dw_2.Object.paen_numero[1]

SELECT	IsNull(Max(pafr_secuen), 0)
	INTO	:li_Secuencia
	FROM	dbo.palletfruta_trans
	WHERE	clie_codigo	=	:li_cliente
	AND	plde_codigo =	:li_planta
	AND	paen_numero	=	:ll_numero;

FOR ll_fila = 1 TO dw_1.RowCount()
		dw_1.Object.clie_codigo[ll_fila]	=	dw_2.Object.clie_codigo[1]
		dw_1.Object.plde_codigo[ll_fila]	=	dw_2.Object.plde_codigo[1]
		dw_1.Object.paen_numero[ll_fila]	=	dw_2.Object.paen_numero[1]
		dw_1.Object.espe_codigo[ll_fila]	=	dw_2.Object.espe_codigo[1]
		dw_1.Object.cond_codigo[ll_fila]	=	dw_2.Object.cond_codigo[1]
		dw_1.Object.etiq_codigo[ll_fila]	=	dw_2.Object.etiq_codigo[1]
		//dw_1.Object.pafr_fecemb[ll_fila]	=	dw_2.Object.paen_fecemb[1]
		
//		dw_1.Object.pafr_prdrot[ll_fila]	=	dw_1.Object.prod_codigo[1]
//		dw_1.Object.pafr_calrot[ll_fila]	=	dw_1.Object.pafr_calibr[1]
						
		IF IsNull(dw_2.Object.paen_pmixto[1]) THEN
			li_paen_mixto = 0
		ELSE
			li_paen_mixto = dw_2.Object.paen_pmixto[1]
		END IF
		
		IF dw_1.Object.pafr_secuen[ll_fila]	< 9999 THEN
			IF li_paen_mixto <> 1 THEN
//				dw_1.Object.vari_codigo[ll_fila]	=	dw_2.Object.vari_codigo[1]
//				dw_1.Object.emba_codigo[ll_fila]	=	dw_2.Object.emba_codigo[1]
//				//dw_1.Object.pafr_embrea[ll_fila]	=	dw_2.Object.emba_codigo[1]				
//				dw_1.Object.pafr_varrot[ll_fila]	=	iuo_variedadrotula.Varirotula
			END IF
		END IF
		
	IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
		li_Secuencia ++
		dw_1.Object.pafr_secuen[ll_Fila] =	li_Secuencia
	END IF	
	ll_CajasDet	= ll_CajasDet + dw_1.Object.pafr_ccajas[ll_fila]
NEXT

IF dw_2.Object.paen_ccajas[1] > ll_CajasDet THEN
	li_respue	=	MessageBox("ADVERTENCIA","La Suma de Cajas del Detalle del Pallet~r" + &
							"es Menor a la Especificada.~r~rDesea Grabar.", &
							Question!, YesNo!)
	IF li_respue <> 1 THEN
		Message.DoubleParm = -1
	END IF	
END IF

IF dw_2.Object.paen_tipopa[1] = 1 THEN
	IF isnull(dw_2.Object.tpem_codigo[1]) OR dw_2.Object.tpem_codigo[1] = '' THEN
		MessageBox("Atención","Tipo Pallet No Puede ser NULO", Exclamation!, Ok!)
		Message.DoubleParm = -1
		dw_2.SetItem(1, "tpem_codigo", String(ll_null))
		Return 
	END IF
END IF
end event

event ue_modifica_detalle;Cuentacajas()

IF ii_bloqdaw = 0 THEN
	IF dw_1.RowCount() > 0 THEN
		istr_mant.agrega = False
		
		OpenWithParm(iw_mantencion, istr_mant)
	END IF
END IF	
end event

event ue_borrar;Integer	li_Cliente, li_Planta
Long		ll_Numero, ll_recepcion

li_Cliente		=	Integer(istr_mant.argumento[1])
ll_Numero		=	Long(istr_mant.argumento[2])
li_Planta		=	Integer(istr_mant.argumento[6])

IF dw_2.RowCount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

/*DELETE dbo.Histcontcalidad
WHERE clie_codigo = :li_Cliente
AND   plde_codigo = :li_Planta
AND   paen_numero = :ll_Numero;*/

SELECT rfpe_numero
INTO :ll_recepcion
FROM dbo.Recfruproced_trans
WHERE clie_codigo = :li_Cliente
AND   plde_codigo = :li_Planta
AND   paen_numero = :ll_Numero;

DELETE dbo.Recfruproced_trans
WHERE clie_codigo = :li_Cliente
AND   plde_codigo = :li_Planta
AND   paen_numero = :ll_Numero;

w_main.SetMicroHelp("Borrando Registro...")

DELETE dbo.palletfruta_trans
WHERE clie_codigo = :li_Cliente
AND   plde_codigo = :li_Planta
AND   paen_numero = :ll_Numero;

DELETE dbo.palletencab_trans
WHERE clie_codigo = :li_Cliente
AND   plde_codigo = :li_Planta
AND   paen_numero = :ll_Numero;

commit;

//IF dw_1.RowCount() > 0 THEN dw_1.RowsMove(1,dw_1.RowCount(),Primary!,dw_1,1,Delete!)

//IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		
	//	IF wf_actualiza_db(True) THEN
			w_main.SetMicroHelp("Registro Borrado...")
			dw_1.Reset()
			dw_2.Reset()
			This.TriggerEvent("ue_nuevo")
			SetPointer(Arrow!)
	//	ELSE
		//	w_main.SetMicroHelp("Registro no Borrado...")
	//	END IF			
//ELSE
//	ib_borrar = False
//	MessageBox(This.Title,"No se puede borrar actual registro.")
//END IF
end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	IF ii_estado<>2 AND ii_estado<>3 THEN pb_Eliminar.Enabled	=	True
	
	IF istr_mant.Solo_Consulta THEN
		dw_2.Enabled			=	False
//		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled	=	False
		pb_ins_det.Enabled	=	False
		pb_eli_det.Enabled	=	False
	ELSE
		IF ii_bloqdaw = 0 THEN
			dw_2.Enabled			=	True
			pb_Eliminar.Enabled	=	True
			pb_Grabar.Enabled		=	True
			pb_ins_det.Enabled	=	False
			IF dw_1.RowCount() > 1 THEN
				pb_eli_det.Enabled	=	True
			ELSE
				pb_eli_det.Enabled	=	False				
			END IF	
			pb_imprimir.Enabled  =  False
		ELSE
			dw_2.Enabled			=	False
			pb_Eliminar.Enabled	=	False
			pb_Grabar.Enabled		=	False
			pb_ins_det.Enabled	=	False
			pb_eli_det.Enabled	=	False
			pb_imprimir.Enabled  =  True
		END IF	
	END IF
ELSE
	IF istr_mant.Solo_Consulta THEN
		pb_ins_det.Enabled	=	False
	ELSEIF dw_1.RowCount() > 1 THEN
		pb_ins_det.Enabled	=	True
	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

event ue_guardar;Boolean lb_Borrado = False

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

// Borra palletencabhisto,palletfrutahisto
IF  ii_ancestro = 1 THEN
	IF wf_actualiza_db(True)  THEN
		lb_Borrado	=	True
	END IF
END IF

IF ( ii_ancestro = 0 OR (ii_ancestro = 1 AND lb_Borrado) ) THEN
	IF wf_actualiza_db(False) THEN
		w_main.SetMicroHelp("Información Grabada.")
		pb_eliminar.Enabled	= True
		pb_imprimir.Enabled	= True
		TriggerEvent("ue_nuevo")
	END IF
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event resize;call super::resize;pb_recupera.x	= pb_salir.x
pb_recupera.y	= pb_salir.y + pb_salir.Height + 10
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_modificapallet_rotuladotrans
integer x = 82
integer y = 1140
integer width = 3099
integer height = 752
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_palletfruta_rotulatrans"
end type

event dw_1::sqlpreview;//
end event

event dw_1::updatestart;//
end event

event dw_1::updateend;//
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_modificapallet_rotuladotrans
integer x = 73
integer y = 56
integer width = 2926
integer height = 1036
string dataobject = "dw_mant_palletencab_trans_modifica"
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null, ll_PalletMan, ll_Data
String	ls_columna, ls_asda
Date		ld_nula
Integer	li_FilaMan

DataWIndowChild	dw_calibres

SetNull(ll_null)
SetNull(ld_nula)

ls_columna = GetColumnName()

ii_yaexiste	=	0

CHOOSE CASE ls_columna
	CASE "clie_codigo"
		
		IF F_ValidaCliente(Integer(data)) THEN
			istr_mant.argumento[1]	= data
			cliente_rotulado(Integer(Data))
			dw_especie.Retrieve()
			dw_etiqueta.Retrieve()
			
			dw_2.GetChild("plde_codigo", dw_planta)
			dw_planta.SetTransObject(sqlca)
			dw_planta.Retrieve(1)
			
			IF EmbalajeCliente(Integer(data)) = "" THEN
				MessageBox("Atención", "Cliente no tiene definido Tipos de Pallets" + &
								" por Embalaje.~r~rIngrese o seleccione otro Cliente.")
				dw_2.SetItem(1, "clie_codigo", gi_codexport)
				RETURN 1
			END IF	
		ELSE
			dw_2.SetItem(1, "clie_codigo", gi_codexport)
			cliente_rotulado(Integer(gi_codexport))
			RETURN 1
		END IF
		
	CASE "paen_numero"
		ll_PalletMan = dw_2.Object.paen_numero[1]
		li_FilaMan	 = Integer(GetRow())
		
		istr_mant.argumento[2] = data
		
		IF IsNull(ll_PalletMan) OR ll_PalletMan=0 THEN
			ll_Data = Long(Data)
		ELSE
			ll_Data = ll_PalletMan
		END IF
		
		IF ExistePallet(String(ll_Data)) OR IsNull(ll_Data) THEN
			ii_yaexiste = 1
			dw_2.SetItem(1, "paen_numero", ll_null)
			RETURN 1
		END IF
	
	CASE "espe_codigo"
		istr_mant.argumento[3]	= data
		dw_2.SetItem(1, "vari_nombre", "")
		dw_2.SetItem(1, "vari_codigo", ll_null)
				
	CASE "vari_codigo"
		IF ExisteVariedad(data) = False THEN
			dw_2.SetItem(1, "vari_nombre", "")
			dw_2.SetItem(1, "vari_codigo", ll_null)
			RETURN 1
		ELSE
			istr_mant.argumento[58] = String(existevariedad_relacionada(Integer(istr_mant.argumento[3]),integer(data)))	
		END IF
					
	CASE "plde_codigo"
		istr_mant.argumento[6]	= data
		
	CASE "emba_codigo"
		IF ExisteEmbalaje(data) = False THEN
			dw_2.SetItem(1, "emba_nombre", "")
			dw_2.SetItem(1, "emba_codigo", String(ll_null))
			RETURN 1
		ELSE
			IF dw_2.Object.paen_tipopa[1] = 1 THEN
				dw_2.Object.tpem_codigo.Protect	=	0
				dw_2.Object.paen_ccajas.Protect	=	0
				dw_2.Object.paen_altura.Protect	=	0
				dw_2.Object.tpem_codigo.BackGround.Color = RGB(255,255,255)
				dw_2.Object.paen_ccajas.BackGround.Color = RGB(255,255,255)
				dw_2.Object.paen_altura.BackGround.Color = RGB(255,255,255)
				
				dw_2.GetChild("tpem_codigo", dw_emba)
				dw_emba.SetTransObject(sqlca)
				dw_emba.Retrieve(Integer(istr_mant.argumento[1]),istr_mant.argumento[7])
			END IF
		END IF
			
	CASE "paen_tipopa"
		IF data = '1' THEN
			istr_mant.argumento[7] = dw_2.Object.emba_codigo[1]
			
			dw_2.GetChild("tpem_codigo", dw_emba)
			dw_emba.SetTransObject(sqlca)
			dw_emba.Retrieve(Integer(istr_mant.argumento[1]),istr_mant.argumento[7])
			
			dw_2.Object.paen_altura.Protect	=	0
			dw_2.Object.tpem_codigo.Protect	=	0
			dw_2.Object.paen_ccajas.Protect	=	1
			dw_2.Object.paen_altura.BackGround.Color = RGB(255,255,255)
			dw_2.Object.tpem_codigo.BackGround.Color = RGB(255,255,255)
			dw_2.Object.paen_ccajas.BackGround.Color = RGB(166,180,210)
		ELSE	
			dw_2.Object.paen_altura.Protect	=	1
			dw_2.Object.tpem_codigo.Protect	=	1
			dw_2.Object.paen_ccajas.Protect	=	0
			dw_2.Object.paen_altura.BackGround.Color = RGB(166,180,210)
			dw_2.Object.tpem_codigo.BackGround.Color = RGB(166,180,210)
			dw_2.Object.paen_ccajas.BackGround.Color = RGB(255,255,255)
			dw_2.SetItem(1, "tpem_codigo", String(ll_null))
		END IF	
		
	CASE "tpem_codigo"
		IF dw_2.object.paen_tipopa[Row] = 1 THEN
			IF ExisteTipoEmbalaje(data) = False THEN
				dw_2.SetItem(1, "tpem_codigo", String(ll_null))
				RETURN 1
			END IF
		END IF

	CASE "etiq_codigo"
		istr_mant.argumento[9]	= data
		
	CASE "cond_codigo"
		istr_mant.argumento[10]	= data
			
	CASE "paen_ccajas"
		istr_mant.argumento[11]	= data
			
	CASE "paen_pexpor"
		istr_mant.argumento[22]	= data
			
	CASE "paen_pmixto"
		istr_mant.argumento[23]	= data


	CASE "paen_fecemb"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, "paen_fecemb", ld_nula)
			RETURN 1
		ELSE
			istr_mant.Argumento[40] = Data
		END IF
		
	CASE "paen_cosecha"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, "paen_cosecha", ld_nula)
			RETURN 1
		END IF
		
//	CASE "paen_nrasda"
//		IF data<>'' OR NOT IsNull(data)  THEN
//			istr_mant.argumento[32] = data
//			data = istr_mant.argumento[32]
//		END IF
//		
//		IF Existe_nrasda(data) OR IsNull(data) THEN
//			dw_2.SetItem(1, "paen_nrasda", String(ll_null))
//			RETURN 1
//		END IF
		
END CHOOSE

IF  ii_estado<>2 AND  ii_estado<>3 THEN
	HabilitaIngreso()
END IF
end event

event dw_2::clicked;call super::clicked;IF dw_2.Object.paen_pmixto[1]	=	1 AND Integer(dw_2.Object.paen_pmixto.Protect)	=	1 THEN RETURN

CHOOSE CASE dwo.name
		
	CASE "buscavariedad"
		buscavariedad()
		
	CASE "buscaembalaje"
		buscaembalaje()
		
END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_modificapallet_rotuladotrans
integer x = 3241
integer y = 276
long backcolor = 553648127
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_modificapallet_rotuladotrans
integer x = 3241
integer y = 500
long backcolor = 553648127
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_modificapallet_rotuladotrans
integer x = 3241
integer y = 708
fontcharset fontcharset = ansi!
fontfamily fontfamily = decorative!
string facename = "Algerian"
long backcolor = 553648127
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_modificapallet_rotuladotrans
integer x = 3241
integer y = 916
long backcolor = 553648127
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_modificapallet_rotuladotrans
integer x = 3241
integer y = 1176
long backcolor = 553648127
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_modificapallet_rotuladotrans
boolean visible = false
integer x = 3241
integer y = 1504
long backcolor = 553648127
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_modificapallet_rotuladotrans
integer x = 3241
integer y = 1680
long backcolor = 553648127
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_modificapallet_rotuladotrans
integer x = 3241
integer y = 96
long backcolor = 553648127
end type

event pb_buscar::clicked;//
end event

type pb_recupera from picturebutton within w_maed_modificapallet_rotuladotrans
string tag = "Caja a Caja"
integer x = 3232
integer y = 1392
integer width = 302
integer height = 244
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\apuntes.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\apuntes-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Long	ll_fila_d	

ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]), &
												 Integer(istr_mant.argumento[6]))
IF ll_fila_d > 0 THEN
	MessageBox("Atención", "Los Cambios Surtiran Efecto Solo Cuando Haya Grabado.", Exclamation!, OK!)
	OpenWithParm(w_mant_deta_palletagrupado, istr_mant)
END IF
end event

type dw_3 from datawindow within w_maed_modificapallet_rotuladotrans
boolean visible = false
integer x = 3584
integer y = 108
integer width = 201
integer height = 160
integer taborder = 140
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_palletfruta_trans"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

