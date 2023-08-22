$PBExportHeader$w_maed_palletencab_rotulamodifica.srw
forward
global type w_maed_palletencab_rotulamodifica from w_mant_encab_deta_csd
end type
type dw_histofruta from datawindow within w_maed_palletencab_rotulamodifica
end type
type dw_histoencab from datawindow within w_maed_palletencab_rotulamodifica
end type
end forward

shared variables

end variables

global type w_maed_palletencab_rotulamodifica from w_mant_encab_deta_csd
integer width = 3826
integer height = 2028
string menuname = ""
event ue_imprimir ( )
dw_histofruta dw_histofruta
dw_histoencab dw_histoencab
end type
global w_maed_palletencab_rotulamodifica w_maed_palletencab_rotulamodifica

type variables
Integer	ii_yaexiste, ii_estado, ii_ancestro, ii_Proceso, ii_bloquea
Long		il_NumProce
Boolean	ib_Existe

w_mant_deta_palletfruta_rotulamodifica iw_mantencion

DataWindowChild	dw_especie, dw_etiqueta, dw_planta,&
                  dw_cliente,dw_condiciones,dw_emba
						
DataStore	ids_Palletencabhisto,ids_Palletfrutahisto

uo_variedadrotula			iuo_variedadrotula
uo_responablescierre		iuo_responablescierre
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
public function boolean grabahistoria ()
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

lstr_busq.argum[1]	=	string(dw_2.Object.espe_codigo[1])//istr_mant.Argumento[1]
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
	dw_2.Object.plde_codigo.Color 	= 0
	
	dw_2.Object.clie_codigo.BackGround.Color 		= RGB(255,255,255)
	dw_2.Object.paen_numero.BackGround.Color	= RGB(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color 	= RGB(255,255,255)
	dw_2.SetColumn("clie_codigo")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clie_codigo.Protect		=	1
	dw_2.Object.paen_numero.Protect	=	1
	dw_2.Object.plde_codigo.Protect		=	1

	dw_2.Object.clie_codigo.Color		= RGB(255,255,255)
	dw_2.Object.paen_numero.Color	= RGB(255,255,255)
	dw_2.Object.plde_codigo.Color 	= RGB(255,255,255)

	dw_2.Object.clie_codigo.BackGround.Color		= 553648127
	dw_2.Object.paen_numero.BackGround.Color	= 553648127
	dw_2.Object.plde_codigo.BackGround.Color 	= 553648127
	
	IF dw_2.Object.paen_pmixto[1]	=	1 AND Integer(dw_2.Object.paen_pmixto.Protect)	=	1 THEN
		dw_2.Object.vari_codigo.Protect	=	1
		dw_2.Object.emba_codigo.Protect	=	1
		dw_2.Object.vari_codigo.Color 		= RGB(255,255,255)
		dw_2.Object.emba_codigo.Color	= RGB(255,255,255)
		
		dw_2.Object.vari_codigo.BackGround.Color 		= 553648127
		dw_2.Object.emba_codigo.BackGround.Color	= 553648127
	END IF
	IF ii_Ancestro = 1 THEN
		dw_2.Object.cond_codigo.Protect				=	1
		dw_2.Object.cond_codigo.Color 				= RGB(255,255,255)
		dw_2.Object.cond_codigo.BackGround.Color= 553648127
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

IF	IsNull(dw_2.Object.dest_codigo[1]) OR dw_2.Object.dest_codigo[1] = 0 THEN
	lb_estado = False
END IF

/* Valida que fecha ingresada este dentro del rango*/
IF lb_estado = TRUE THEN
	IF NOT controla_fecha(dw_2.Object.paen_fecemb[1]) THEN
		lb_estado = False
	ELSEIF NOT controla_fecha(dw_2.Object.paen_cosecha[1]) THEN
		lb_estado = False
	END IF
END IF

IF ii_bloquea = 0 THEN
	pb_ins_det.Enabled = True
	pb_eliminar.Enabled = True
ELSE	
	pb_ins_det.Enabled = False
	pb_eliminar.Enabled = False	
END IF	
end subroutine

public function boolean existepallet (string ls_columna);Integer	li_cliente, li_planta, li_Movto
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
		FROM	dbo.palletfruta
		WHERE	clie_codigo	= 	:li_cliente
		AND	paen_numero = 	:ll_nropal
		AND	plde_codigo	=	:li_planta
		AND   pafr_secuen >  99999;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla spro_palletencab")
		lb_retorno = True
	ELSEIF ll_pcopda > 1 THEN	
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

public function boolean grabahistoria ();Boolean 	lb_Retorno
Integer 	li_Respuesta,li_cliente,li_planta,li_Proceso
Long 	  	ll_Nropal
Date 		ld_Fecha
String	ls_Observa

lb_Retorno	=	False

li_cliente	=	Integer(dw_2.GetItemNumber(1, "clie_codigo"))
li_planta	=	Integer(dw_2.GetItemNumber(1, "plde_codigo"))
ll_Nropal	=	dw_2.Object.paen_numero[1]

ld_Fecha		=	Today() 
ls_Observa 	= 	'Recepción por Mantención';    
li_Proceso	= 	5

DECLARE TraspasaHistoriadesdeProceso PROCEDURE FOR dbo.Fproc_TraspasaHistoriadesdeProceso
			@Cliente 	=	:li_cliente, 
			@Planta 		=	:li_planta,
			@Pallet 		=	:ll_Nropal, 
			@Fecha		=	:ld_Fecha,
			@Observa		=	:ls_Observa,
			@Proceso		=  :li_Proceso;

li_Respuesta	=	1				
IF	li_Respuesta = 1 THEN

	EXECUTE TraspasaHistoriadesdeProceso ;
		
	IF SQLCA.SQLCode < 0 THEN
		MessageBox("Error en Grabar Historia", "Se ha producido un Error en Proceso ." + &
						SQLCA.SQLErrText)							
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
	END IF
END IF

RETURN lb_Retorno
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

If Not dw_2.uf_check_required(0) Then RETURN False

If Not dw_1.uf_validate(0) Then RETURN False

lb_AutoCommit		=	SQLCA.AutoCommit
sqlca.AutoCommit	=	False

If Borrando Then
	If ids_PalletFrutaHisto.Update(True, False) = 1 Then
		If ids_PalletEncabHisto.Update(True, False) = 1 Then
			Commit;
			
			If SQLCA.SQLCode <> 0 Then
				F_ErrorBaseDatos(SQLCA, This.Title)
				RollBack;
			Else
				lb_Retorno	=	True
				
				ids_PalletFrutaHisto.ResetUpdate()
				ids_PalletEncabHisto.ResetUpdate()							
			End If
		Else
			F_ErrorBaseDatos(SQLCA, This.Title)
			RollBack;
		End If
	Else
		F_ErrorBaseDatos(SQLCA, This.Title)
		RollBack;
	End If	
Else
	If dw_2.Update(True, False) = 1 Then
		If dw_1.Update(True, False) = 1 Then
			If dw_histoEncab.Update(True, False) = 1 Then
				If dw_histoFruta.Update(True, False) = 1 Then
					Commit;
					 
					If SQLCA.SQLCode <> 0 Then
						F_ErrorBaseDatos(SQLCA, This.Title)
						RollBack;
					Else
						lb_Retorno	=	True
						
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
						dw_histoEncab.ResetUpdate()
						dw_histoFruta.ResetUpdate()							
					End If
				Else
					F_ErrorBaseDatos(SQLCA, This.Title)
					RollBack;
				End If
			Else
				F_ErrorBaseDatos(SQLCA, This.Title)
				RollBack;
			End If
		Else
			F_ErrorBaseDatos(SQLCA, This.Title)
			RollBack;
		End If
	Else
		F_ErrorBaseDatos(SQLCA, This.Title)
		RollBack;
	End If
End If

SQLCA.AutoCommit	=	lb_AutoCommit

Return lb_Retorno
end function

public function boolean buscaingreso ();Integer	li_Cliente, li_Planta, li_Cuenta, li_Cuenta2, li_Cuenta3 ,li_Cuenta4, &
			li_Cuenta5, li_Cuenta1, li_TipoEn, li_Origen, li_Cuenta6
Long		ll_Numero, ll_Folio, ll_rfpe_numero
String	ls_vacia

li_Cliente	=	dw_2.Object.clie_codigo[1]
li_Planta	=	dw_2.Object.plde_codigo[1]
ll_Numero	=	Long(istr_mant.argumento[2]) //dw_2.Object.paen_numero[1]
ls_vacia		=	istr_mant.argumento[21]

//cambio solicitado por rancagua para modificar el tipo de frio y tipo pallet
//14/05/2008

//Nuevo cambio 07/01/2009 se solicita bloquear todo en caso que hayan procesos de por medio
//solicitado por gabriel en la agricola

SELECT 	Count(*) INTO :li_Cuenta
  FROM 	dbo.recfruproced
  WHERE 	clie_codigo = :li_Cliente
  AND   	plde_codigo = :li_Planta
  AND   	paen_numero = :ll_Numero;
  
//IF li_Cuenta = 0 THEN li_Cuenta6 = 1

li_Cuenta = 1

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura Tabla Recfruproced")
   RETURN False
ELSEIF li_Cuenta = 0 THEN
	MessageBox("Atención","Número de Pallet No Generado en Recepción. Ingrese Otro.", Exclamation!, Ok!)
	RETURN False	 
ELSEIF li_Cuenta > 0 THEN
	 SELECT  rfpe_numero 
		INTO  :ll_rfpe_numero
		FROM  dbo.recfruproced
		WHERE clie_codigo = :li_Cliente
		AND   plde_codigo = :li_Planta
		AND   paen_numero = :ll_Numero;
		
	 SELECT	rfpe_tipoen, rfpe_ptaori
		INTO 	:li_TipoEn, :li_Origen
		FROM 	dbo.recfruprocee
		WHERE plde_codigo = :li_Planta
		AND   rfpe_numero = :ll_rfpe_numero;
	 
	 IF li_TipoEn = 1  THEN
		ii_Proceso = 1
		IF IsNull(ls_vacia) OR ls_vacia= '' THEN
			 istr_mant.argumento[21]	=	String(li_Origen)
		END IF
	 ELSE 
		 ii_Proceso = 2			 
	 END IF	
	 ///////////////////////////////////////	

	 SELECT 	pafh_proces 
		INTO 	:ll_Folio
		FROM 	dbo.palletfrutahisto
		WHERE	clie_codigo   = :li_Cliente
		AND  	plde_codigo   = :li_Planta 
		AND	paen_numero   = :ll_Numero 
		AND	pafr_tipdoc   = :ii_Proceso;		
	 //////////////////////////////////////
		
//			 SELECT	Count() INTO :li_Cuenta1
//				FROM 	dbo.alpalletfruta
//				WHERE clie_codigo   = :li_Cliente
//				AND	plde_codigo   = :li_Planta   
//				AND 	paen_numero   = :ll_Numero
//				AND   altu_numero   <> :ll_Folio;
		
	 SELECT Count(*) INTO :li_Cuenta2
		FROM dbo.Inspecpaldet
		WHERE clie_codigo = :li_Cliente
		AND   plde_codigo = :li_Planta
		AND   paen_numero = :ll_Numero;
	 SELECT Count(*) INTO :li_Cuenta3
		FROM dbo.Fumigadet
		WHERE clie_codigo = :li_Cliente
		AND   plde_codigo = :li_Planta
		AND   paen_numero = :ll_Numero;	
	 SELECT Count(*) INTO :li_Cuenta4
		FROM dbo.Repalletdeta
		WHERE clie_codigo = :li_Cliente
		AND   plde_codigo = :li_Planta
		AND   (paen_numero = :ll_Numero OR
		paen_nroori = :ll_Numero);				
	 SELECT Count(*) INTO :li_Cuenta5 
		FROM dbo.Despafrigode
		WHERE clie_codigo = :li_Cliente
		AND   plde_codigo = :li_Planta
		AND   paen_numero = :ll_Numero;
		
	 IF IsNull(li_Cuenta1) THEN li_Cuenta1 = 0
	 IF IsNull(li_Cuenta2) THEN li_Cuenta2 = 0
	 IF IsNull(li_Cuenta3) THEN li_Cuenta3 = 0
	 IF IsNull(li_Cuenta4) THEN li_Cuenta4 = 0
	 IF IsNull(li_Cuenta5) THEN li_Cuenta5 = 0
	 IF IsNull(li_Cuenta6) THEN li_Cuenta6 = 0

	 IF (li_Cuenta1 + li_Cuenta2 + li_Cuenta3 + li_Cuenta4 + li_Cuenta5 + li_Cuenta6) > 0 THEN
			//IF li_Cuenta6 = 0 THEN
				MessageBox("Atención","Número de Pallet de Recepción tiene procesos adicionales. Ingrese Otro.", Exclamation!, Ok!)
				RETURN False
			//END IF	
			ii_bloquea = 1  
	 ELSE
		ii_bloquea = 0
		RETURN True
	 END IF
END IF	
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
	dw_2.Object.clie_codigo.Protect	=	0   
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
	dw_2.Object.paen_inspec.Protect	=	1	//dw_2.Object.paen_inspec.Protect	=	0
	dw_2.Object.dest_codigo.Protect	=	0
	dw_2.Object.paen_pexpor.Protect	=	0
	dw_2.Object.paen_pmixto.Protect	=	0
	dw_2.Object.paen_nrasda.Protect	=	0
	dw_2.Object.copa_codigo.Protect	=	0
	dw_2.Object.paen_fecemb.Protect	=	0
	dw_2.Object.paen_cosecha.Protect	=	0
	
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

	dw_2.Object.cond_codigo.Color 	= Rgb(255,255,255)
	dw_2.Object.paen_concal.Color 	= Rgb(255,255,255)
	dw_2.Object.paen_inspec.Color 	= Rgb(255,255,255)

	dw_2.Object.cond_codigo.BackGround.Color 	= 553648127
	dw_2.Object.paen_concal.BackGround.Color 	= 553648127
	dw_2.Object.paen_inspec.BackGround.Color 	= 553648127
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

	dw_2.Enabled = False
	pb_eliminar.Enabled	= False
	pb_ins_det.Enabled 	= False
	pb_eli_det.Enabled 	= False
	pb_grabar.Enabled		= False
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
This.Height	=	2500
im_menu		= m_principal

If Not f_validafechatempo(today()) Then Messagebox('Atención','Aclare Fecha Actual Temporada con Informática')

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

If gi_CodExport = 300 Then dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.ModIfy("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.ModIfy("DataWindow.Footer.Height = 110")

istr_mant.dw					=	dw_1
istr_mant.solo_consulta		=	False
istr_mant.argumento[14]	=	''
istr_mant.argumento[16]	=	''

pb_nuevo.PostEvent(Clicked!)
ls_movto	= Message.StringParm

ii_Proceso		=	0
ii_ancestro		= 	0

If ls_movto = "" Then 
	ii_ancestro	= 	1
	GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Mantención de Pallet Histórico", 1)
End If 

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

istr_mant.argumento[1]		=	String(gi_CodExport)
istr_mant.argumento[6]		=	String(gi_CodPlanta)
istr_mant.argumento[7]		=	gs_CodEmbalaje
istr_mant.argumento[22]	=	'1'		//	Pallet Exportación
istr_mant.argumento[23]	=	'0'		//	Pallet Mixto

dw_2.Object.paen_pmixto.Protect	=	1

If ii_ancestro = 1 Then
	// Para guardar  Palletencabhisto y Palletfrutahisto
	ids_Palletencabhisto					=	CREATE	DataStore
	ids_Palletfrutahisto					=	CREATE	DataStore

	ids_PalletEncabhisto.DataObject	=	'dw_mant_palletencabhisto_proceso'
	ids_PalletFrutaHisto.DataObject		=	'dw_mant_palletfrutahisto_proceso'
	
	ids_PalletEncabhisto.SetTransObject(sqlca)
	ids_PalletFrutaHisto.SetTransObject(sqlca)	
	dw_histoEncab.SetTransObject(sqlca)
	dw_histoFruta.SetTransObject(sqlca)
End If

iuo_variedadrotula			=	Create uo_variedadrotula
iuo_responablescierre	=	Create uo_responablescierre

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
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_nuevo_detalle;istr_mant.borra	= False
istr_mant.agrega	= True
cuentacajas()
istr_mant.argumento[28]=String(dw_2.object.paen_fecemb[1])
//istr_mant.argumento[21]=String(dw_2.object.plde_codigo[1])
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
	
	If ll_fila_e = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos 1.", &
										Information!, RetryCancel!)
	Else
		istr_mant.Argumento[22]	=	String(dw_2.Object.paen_pexpor[1])
		istr_mant.Argumento[23]	=	String(dw_2.Object.paen_pmixto[1])
		istr_mant.argumento[40] 	=  String(dw_2.Object.paen_fecemb[1])
		ii_estado							=	dw_2.Object.paen_estado[1]
		
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]), &
												 Integer(istr_mant.argumento[6]))

			If ii_ancestro = 1 Then
				ll_fila_fh	= ids_Palletfrutahisto.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]), &
																		  Integer(istr_mant.argumento[6]),ii_Proceso)	 //1								 
				If ll_fila_fh > 0 Then
					il_NumProce	=	ids_Palletfrutahisto.Object.pafh_proces[1]
					ll_fila_eh	=	ids_Palletencabhisto.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]), &
																		  Integer(istr_mant.argumento[6]),il_NumProce)		
				End If
			End If

			If ll_fila_d = -1 Then
				respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos 2.", Information!, RetryCancel!)
			Else
				/*
				Chequea Tipo Usuario
				*/
				If Not iuo_ResponablesCierre.Existe(gi_codplanta,gstr_us.Nombre,False,Sqlca) Then
					ii_bloquea = 1
					dw_1.Enabled = False
					dw_2.Enabled = False
					pb_eliminar.Enabled	= False
					pb_ins_det.Enabled 	= False
					pb_eli_det.Enabled 	= False
					pb_grabar.Enabled		= False
				Else
					ii_bloquea = 0
				End If
								
				pb_imprimir.Enabled	= True
				If ii_estado=2 OR ii_estado=3 Then
					istr_mant.solo_consulta	=	True	
				Else
					istr_mant.solo_consulta	=	False
					pb_eliminar.Enabled		= True
					pb_grabar.Enabled		= True				
					pb_ins_det.Enabled		= True
				End If
				If ll_fila_d > 0 Then
					If  ii_estado=2 OR ii_estado=3 Then
						pb_eli_det.Enabled = False
					Else						
						dw_1.SetRow(1)
						dw_1.SelectRow(1,True)
						dw_1.SetFocus()
						HabilitaEncab(False)
					End If
				Else
					If  ii_estado<>2 AND ii_estado<>3 Then pb_ins_det.SetFocus()
				End If
			End If
		LOOP WHILE respuesta = 1

		If respuesta = 2 Then Close(This)
	End If
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

If respuesta = 2 Then Close(This)
end event

on w_maed_palletencab_rotulamodifica.create
int iCurrent
call super::create
this.dw_histofruta=create dw_histofruta
this.dw_histoencab=create dw_histoencab
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_histofruta
this.Control[iCurrent+2]=this.dw_histoencab
end on

on w_maed_palletencab_rotulamodifica.destroy
call super::destroy
destroy(this.dw_histofruta)
destroy(this.dw_histoencab)
end on

event ue_nuevo;call super::ue_nuevo;HabilitaEncab(True)

dw_2.SetItem(1, "clie_codigo", Integer(istr_mant.Argumento[1]))
dw_2.SetItem(1, "plde_codigo", Integer(istr_mant.Argumento[6]))
dw_2.SetItem(1, "espe_codigo", gi_CodEspecie)

istr_mant.argumento[3]		=	String(gi_CodEspecie)
istr_mant.argumento[9]		=	"1"
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

iuo_variedadrotula.existe(dw_2.Object.espe_codigo[1],dw_2.Object.vari_codigo[1], True, sqlca) 

dw_2.Object.paen_varrot[1] = iuo_variedadrotula.Varirotula
//dw_2.Object.paen_nrasda[1] = Istr_mant.argumento[32]

li_cliente	=	dw_2.Object.clie_codigo[1]
li_planta	=	dw_2.Object.plde_codigo[1]
ll_numero	=	dw_2.Object.paen_numero[1]

SELECT	IsNull(Max(pafr_secuen), 0)
	INTO	:li_Secuencia
	FROM	dbo.palletfruta
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
								
		If IsNull(dw_2.Object.paen_pmixto[1]) Then
			li_paen_mixto = 0
		Else
			li_paen_mixto = dw_2.Object.paen_pmixto[1]
		End If
					
	If dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModIfied! Then
		li_Secuencia ++
		dw_1.Object.pafr_secuen[ll_Fila] =	li_Secuencia
	End If	
	ll_CajasDet	= ll_CajasDet + dw_1.Object.pafr_ccajas[ll_fila]
NEXT

If dw_2.Object.paen_ccajas[1] > ll_CajasDet Then
	li_respue	=	MessageBox("ADVERTENCIA","La Suma de Cajas del Detalle del Pallet~r" + &
							"es Menor a la EspecIficada.~r~rDesea Grabar.", &
							Question!, YesNo!)
	If li_respue <> 1 Then
		Message.DoubleParm = -1
	End If	
End If

If dw_2.Object.paen_tipopa[1] = 1 Then
	If isnull(dw_2.Object.tpem_codigo[1]) OR dw_2.Object.tpem_codigo[1] = '' Then
		MessageBox("Atención","Tipo Pallet No Puede ser NULO", Exclamation!, Ok!)
		Message.DoubleParm = -1
		dw_2.SetItem(1, "tpem_codigo", String(ll_null))
		Return 
	End If
End If	

//dw_2.SetItem(1, "paen_ccajas", dw_1.Object.total_cajas[1])
If ii_ancestro = 1 Then
	dw_2.Object.paen_pcopda[1]	=	1
	dw_2.Object.paen_usumod[1]	=	gstr_us.Nombre
	dw_2.Object.paen_estmod[1]	=	gstr_us.Computador
		
	dw_histoEncab.Reset()
	dw_histoFruta.Reset()
	ids_PalletEncabHisto.ResetUpdate()
	ids_PalletFrutaHisto.ResetUpdate()
	
	If dw_2.RowCount() > 0 AND dw_1.RowCount() > 0 Then
		ids_PalletEncabHisto.RowsMove(1,ids_Palletencabhisto.RowCount(),Primary!,ids_PalletEncabHisto,1,Delete!)	
		ids_PalletFrutaHisto.RowsMove(1,ids_Palletfrutahisto.RowCount(),Primary!,ids_PalletFrutaHisto,1,Delete!)	
		
		ll_fila_P = dw_histoEncab.InsertRow(0)	
		dw_histoEncab.Object.clie_codigo[ll_fila_P]		=	dw_2.Object.clie_codigo[1]	
		dw_histoEncab.Object.paen_numero[ll_fila_P]	=	dw_2.Object.paen_numero[1]	
		dw_histoEncab.Object.plde_codigo[ll_fila_P]	=	dw_2.Object.plde_codigo[1]	
		dw_histoEncab.Object.pahi_tipopa[ll_fila_P]		=	dw_2.Object.paen_tipopa[1]	
		dw_histoEncab.Object.tpem_codigo[ll_fila_P]	=	dw_2.Object.tpem_codigo[1]	
		dw_histoEncab.Object.espe_codigo[ll_fila_P]	=	dw_2.Object.espe_codigo[1]	
		dw_histoEncab.Object.vari_codigo[ll_fila_P]		=	dw_2.Object.vari_codigo[1]	
		dw_histoEncab.Object.tiem_codigo[ll_fila_P]	=	dw_2.Object.tiem_codigo[1]	
		dw_histoEncab.Object.emba_codigo[ll_fila_P]	=	dw_2.Object.emba_codigo[1]	
		dw_histoEncab.Object.cate_codigo[ll_fila_P]	=	dw_2.Object.cate_codigo[1]	
		dw_histoEncab.Object.etiq_codigo[ll_fila_P]		=	dw_2.Object.etiq_codigo[1]	
		dw_histoEncab.Object.stat_codigo[ll_fila_P]		=	dw_2.Object.stat_codigo[1]	
		dw_histoEncab.Object.trat_codigo[ll_fila_P]		=	dw_2.Object.trat_codigo[1]	
		dw_histoEncab.Object.frio_codigo[ll_fila_P]		=	dw_2.Object.frio_codigo[1]	
		dw_histoEncab.Object.cond_codigo[ll_fila_P]	=	dw_2.Object.cond_codigo[1]	
		dw_histoEncab.Object.dest_codigo[ll_fila_P]	=	dw_2.Object.dest_codigo[1]	
		dw_histoEncab.Object.pahi_fecemb[ll_fila_P]	=	dw_2.Object.paen_fecemb[1]	
		dw_histoEncab.Object.pahi_cosecha[ll_fila_P]	=	dw_2.Object.paen_cosecha[1]	
		dw_histoEncab.Object.paen_altura[ll_fila_P]	=	dw_2.Object.paen_altura[1]	
		dw_histoEncab.Object.paen_ccajas[ll_fila_P]	=	dw_2.Object.paen_ccajas[1]	
		dw_histoEncab.Object.tmvp_codigo[ll_fila_P]	=	dw_2.Object.tmvp_codigo[1]	
		dw_histoEncab.Object.paen_fecini[ll_fila_P]		=	dw_2.Object.paen_fecini[1]	
		dw_histoEncab.Object.paen_horain[ll_fila_P]	=	dw_2.Object.paen_horain[1]	
		dw_histoEncab.Object.cama_codigo[ll_fila_P]	=	dw_2.Object.cama_codigo[1]	
		dw_histoEncab.Object.pahi_calle[ll_fila_P]		=	dw_2.Object.paen_calle[1]	
		dw_histoEncab.Object.pahi_base[ll_fila_P]		=	dw_2.Object.paen_base[1]	
		dw_histoEncab.Object.pahi_posici[ll_fila_P]		=	dw_2.Object.paen_posici[1]	
		dw_histoEncab.Object.pahi_estado[ll_fila_P]	=	dw_2.Object.paen_estado[1]	
		dw_histoEncab.Object.pahi_inspec[ll_fila_P]		=	dw_2.Object.paen_inspec[1]	
		dw_histoEncab.Object.pahi_concal[ll_fila_P]		=	dw_2.Object.paen_concal[1]	
		dw_histoEncab.Object.pahi_pexpor[ll_fila_P]	=	dw_2.Object.paen_pexpor[1]	
		dw_histoEncab.Object.pahi_pmixto[ll_fila_P]	=	dw_2.Object.paen_pmixto[1]	
		dw_histoEncab.Object.pahi_proces[ll_fila_P]	=	il_NumProce	
		
		For ll_fila = 1 To dw_1.RowCount()
			ll_fila_P = dw_histoFruta.InsertRow(0)	
			dw_histoFruta.Object.clie_codigo[ll_fila_P]		=	dw_1.Object.clie_codigo[ll_fila]	
			dw_histoFruta.Object.paen_numero[ll_fila_P]	=	dw_1.Object.paen_numero[ll_fila]	
			dw_histoFruta.Object.espe_codigo[ll_fila_P]		=	dw_1.Object.espe_codigo[ll_fila]	
			dw_histoFruta.Object.vari_codigo[ll_fila_P]		=	dw_1.Object.vari_codigo[ll_fila]	
			dw_histoFruta.Object.emba_codigo[ll_fila_P]	=	dw_1.Object.emba_codigo[ll_fila]	
			dw_histoFruta.Object.prod_codigo[ll_fila_P]		=	dw_1.Object.prod_codigo[ll_fila]	
			dw_histoFruta.Object.cond_codigo[ll_fila_P]		=	dw_1.Object.cond_codigo[ll_fila]	
			dw_histoFruta.Object.etiq_codigo[ll_fila_P]		=	dw_1.Object.etiq_codigo[ll_fila]	
			dw_histoFruta.Object.plde_codigo[ll_fila_P]		=	dw_1.Object.plde_codigo[ll_fila]	
			dw_histoFruta.Object.pafh_calibr[ll_fila_P]		=	dw_1.Object.pafr_calibr[ll_fila]	
			dw_histoFruta.Object.pafh_secuen[ll_fila_P]	=	dw_1.Object.pafr_secuen[ll_fila]	
			dw_histoFruta.Object.pafh_ccajas[ll_fila_P]		=	dw_1.Object.pafr_ccajas[ll_fila]	
			dw_histoFruta.Object.pafh_nrlote[ll_fila_P]		=	dw_1.Object.pafr_nrlote[ll_fila]	
			dw_histoFruta.Object.pafh_proces[ll_fila_P]		=	il_NumProce	
			dw_histoFruta.Object.pafr_fecing[ll_fila_P]		=	dw_1.Object.pafr_fecing[ll_fila]	
			dw_histoFruta.Object.pafr_fecemb[ll_fila_P]	=	dw_1.Object.pafr_fecemb[ll_fila]	
			dw_histoFruta.Object.pafr_copack[ll_fila_P]		=	dw_1.Object.pafr_copack[ll_fila]	
			dw_histoFruta.Object.pafr_tipdoc[ll_fila_P]		=ii_Proceso // 1	
			dw_histoFruta.Object.pafr_huert1[ll_fila_P]		=	dw_1.Object.pafr_huert1[ll_fila]
			dw_histoFruta.Object.pafr_cuart1[ll_fila_P]		=	dw_1.Object.pafr_cuart1[ll_fila]
			dw_histoFruta.Object.pafr_varrot[ll_fila_P]		=	dw_1.Object.pafr_varrot[ll_fila]
			dw_histoFruta.Object.pafr_huert4[ll_fila_P]		=	dw_1.Object.pafr_huert4[ll_fila]
			dw_histoFruta.Object.pafr_cuart4[ll_fila_P]		=	dw_1.Object.pafr_cuart4[ll_fila]
			dw_histoFruta.Object.pafr_rotpak[ll_fila_P]		=	dw_1.Object.pafr_rotpak[ll_fila]
			dw_histoFruta.Object.pafr_calrot[ll_fila_P]		=	dw_1.Object.pafr_calrot[ll_fila]
			dw_histoFruta.Object.pafr_prdrot[ll_fila_P]		=	dw_1.Object.pafr_prdrot[ll_fila]
			dw_histoFruta.Object.cate_codigo[ll_fila_P]		=	dw_1.Object.cate_codigo[ll_fila]
			dw_histoFruta.Object.pafr_catrot[ll_fila_P]		=	dw_1.Object.pafr_catrot[ll_fila]
		Next
	End If
End If



end event

event ue_modifica_detalle;Cuentacajas()

IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega = False

//	istr_mant.argumento[4]	=	String(dw_1.Object.vari_codigo[il_fila]) 
//	istr_mant.argumento[7]	=	dw_1.Object.emba_codigo[il_fila] 
	
	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_borrar;Integer	li_Cliente, li_Planta
Long		ll_Numero

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

DELETE dbo.Histcontcalidad
WHERE clie_codigo = :li_Cliente
AND   plde_codigo = :li_Planta
AND   paen_numero = :ll_Numero;

DELETE dbo.Recfruproced
WHERE clie_codigo = :li_Cliente
AND   plde_codigo = :li_Planta
AND   paen_numero = :ll_Numero;

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

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	IF ii_estado<>2 AND ii_estado<>3 THEN 
		IF ii_bloquea = 0 THEN
			pb_Eliminar.Enabled	=	True
		END IF	
	END IF	
	
	IF istr_mant.Solo_Consulta THEN
		dw_2.Enabled			=	False
//		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled	=	False
		pb_ins_det.Enabled	=	False
		pb_eli_det.Enabled	=	False
	ELSE
		IF ii_bloquea = 0 THEN
			dw_2.Enabled			=	True
			pb_Eliminar.Enabled	=	True
			pb_Grabar.Enabled		=	True
			pb_ins_det.Enabled	=	True
			pb_eli_det.Enabled	=	True
		END IF	
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

event ue_guardar;Boolean lb_Borrado = False

If dw_1.AcceptText() = -1 Then RETURN

SetPointer(HourGlass!)
w_main.SetMicroHelp("Grabando información...")
Message.DoubleParm = 0
TriggerEvent("ue_antesguardar")
If Message.DoubleParm = -1 Then RETURN

// Borra palletencabhisto,palletfrutahisto
If  ii_ancestro = 1 Then
	If wf_actualiza_db(True)  Then
		lb_Borrado	=	True
	End If
End If

If ( ii_ancestro = 0 OR (ii_ancestro = 1 AND lb_Borrado) ) Then
	If wf_actualiza_db(False) Then
		w_main.SetMicroHelp("Información Grabada.")
		pb_eliminar.Enabled	= True
		pb_imprimir.Enabled	= True
		TriggerEvent("ue_nuevo")
	End If
Else
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	Return 
End If
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_palletencab_rotulamodifica
integer x = 91
integer y = 1140
integer width = 3045
integer height = 752
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_palletfruta_rotula"
boolean righttoleft = true
end type

event dw_1::clicked;call super::clicked;// 
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_palletencab_rotulamodifica
integer x = 73
integer y = 56
integer width = 2944
integer height = 1052
string dataobject = "dw_mant_palletencab_modifica"
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null, ll_PalletMan, ll_Data
String		ls_columna, ls_asda
Date		ld_nula
Integer	li_FilaMan

DataWindowChild	dw_calibres

SetNull(ll_null)
SetNull(ld_nula)

ls_columna = dwo.Name

ii_yaexiste	=	0

Choose Case ls_columna
	Case "clie_codigo"
		If F_ValidaCliente(Integer(data)) Then
			istr_mant.argumento[1]	= data
			cliente_rotulado(Integer(data))
			dw_especie.Retrieve()
			dw_etiqueta.Retrieve()
			
			dw_2.GetChild("plde_codigo", dw_planta)
			dw_planta.SetTransObject(sqlca)
			dw_planta.Retrieve(1)
			
			If EmbalajeCliente(Integer(data)) = "" Then
				MessageBox("Atención", "Cliente no tiene definido Tipos de Pallets" + &
								" por Embalaje.~r~rIngrese o seleccione otro Cliente.")
				dw_2.SetItem(1, "clie_codigo", gi_codexport)
				Return 1
			End If	
		Else
			dw_2.SetItem(1, "clie_codigo", gi_codexport)
			cliente_rotulado(Integer(gi_codexport))
			Return 1
		End If
		
	Case "paen_numero"
		ll_PalletMan = dw_2.Object.paen_numero[1]
		li_FilaMan	 = Integer(GetRow())
		
		If IsNull(ll_PalletMan) OR ll_PalletMan=0 Then
			ll_Data = Long(Data)
		Else
			ll_Data = ll_PalletMan
		End If
		
		If ExistePallet(String(ll_Data)) OR IsNull(ll_Data) Then
			ii_yaexiste = 1
			dw_2.SetItem(1, "paen_numero", ll_null)
			Return 1
		End If
	
	Case "espe_codigo"
		istr_mant.argumento[3]	= data
		dw_2.SetItem(1, "vari_nombre", "")
		dw_2.SetItem(1, "vari_codigo", ll_null)
				
	Case "vari_codigo"
		If ExisteVariedad(data) = False Then
			dw_2.SetItem(1, "vari_nombre", "")
			dw_2.SetItem(1, "vari_codigo", ll_null)
			Return 1
		Else
			istr_mant.argumento[58] = String(ExisteVariedad_Relacionada(Integer(istr_mant.argumento[3]),integer(data)))	
		End If
					
	Case "plde_codigo"
		istr_mant.argumento[6]	= data
		
	Case "emba_codigo"
		If ExisteEmbalaje(data) = False Then
			dw_2.SetItem(1, "emba_nombre", "")
			dw_2.SetItem(1, "emba_codigo", String(ll_null))
			Return 1
		Else
			If dw_2.Object.paen_tipopa[1] = 1 Then
				dw_2.Object.tpem_codigo.Protect	=	0
				dw_2.Object.paen_ccajas.Protect	=	0
				dw_2.Object.paen_altura.Protect	=	0
				dw_2.Object.tpem_codigo.BackGround.Color = RGB(255,255,255)
				dw_2.Object.paen_ccajas.BackGround.Color = RGB(255,255,255)
				dw_2.Object.paen_altura.BackGround.Color = RGB(255,255,255)
				
				dw_2.GetChild("tpem_codigo", dw_emba)
				dw_emba.SetTransObject(sqlca)
				dw_emba.Retrieve(Integer(istr_mant.argumento[1]),istr_mant.argumento[7])
			End If
			
		End If
			
	Case "paen_tipopa"
		If data = '1' Then
			istr_mant.argumento[7] = dw_2.Object.emba_codigo[1]
			
			dw_2.GetChild("tpem_codigo", dw_emba)
			dw_emba.SetTransObject(sqlca)
			dw_emba.Retrieve(Integer(istr_mant.argumento[1]),istr_mant.argumento[7])
			
			dw_2.Object.paen_altura.Protect	=	0
			dw_2.Object.tpem_codigo.Protect	=	0
			dw_2.Object.paen_ccajas.Protect	=	1
			dw_2.Object.paen_altura.BackGround.Color = RGB(255,255,255)
			dw_2.Object.tpem_codigo.BackGround.Color = RGB(255,255,255)
			dw_2.Object.paen_ccajas.Color = RGB(255,255,255)
			dw_2.Object.paen_ccajas.BackGround.Color = 553648127
		Else	
			dw_2.Object.paen_altura.Protect	=	1
			dw_2.Object.tpem_codigo.Protect	=	1
			dw_2.Object.paen_ccajas.Protect	=	0
			
			dw_2.Object.paen_altura.Color = RGB(255,255,255)
			dw_2.Object.tpem_codigo.Color = RGB(255,255,255)
			
			dw_2.Object.paen_altura.BackGround.Color = 553648127
			dw_2.Object.tpem_codigo.BackGround.Color = 553648127
			dw_2.Object.paen_ccajas.BackGround.Color = RGB(255,255,255)
			dw_2.SetItem(1, "tpem_codigo", String(ll_null))
		End If	
		
	Case "tpem_codigo"
		If dw_2.object.paen_tipopa[Row] = 1 Then
			If ExisteTipoEmbalaje(data) = False Then
				dw_2.SetItem(1, "tpem_codigo", String(ll_null))
				Return 1
			End If
		End If

	Case "etiq_codigo"
		istr_mant.argumento[9]	= data
		
	Case "cond_codigo"
		istr_mant.argumento[10]	= data
			
	Case "paen_ccajas"
		istr_mant.argumento[11]	= data
			
	Case "paen_pexpor"
		istr_mant.argumento[22]	= data
			
	Case "paen_pmixto"
		istr_mant.argumento[23]	= data


	Case "paen_fecemb"
		If Not f_validafechatempo(date(data)) Then
			This.SetItem(Row, "paen_fecemb", ld_nula)
			Return 1
		Else
			istr_mant.Argumento[40] = Data
		End If
		
	Case "paen_cosecha"
		If Not f_validafechatempo(date(data)) Then
			This.SetItem(Row, "paen_cosecha", ld_nula)
			Return 1
		End If
End Choose

If  ii_estado<>2 AND  ii_estado<>3 Then HabilitaIngreso()
end event

event dw_2::clicked;call super::clicked;IF dw_2.Object.paen_pmixto[1]	=	1 AND Integer(dw_2.Object.paen_pmixto.Protect)	=	1 THEN RETURN

CHOOSE CASE dwo.name
	CASE "buscavariedad"
		buscavariedad()
		
	CASE "buscaembalaje"
		buscaembalaje()
		
END CHOOSE
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_palletencab_rotulamodifica
integer x = 3369
integer y = 256
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_palletencab_rotulamodifica
integer x = 3369
integer y = 484
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_palletencab_rotulamodifica
integer x = 3369
integer y = 692
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_palletencab_rotulamodifica
integer x = 3369
integer y = 900
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_palletencab_rotulamodifica
integer x = 3369
integer y = 1160
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_palletencab_rotulamodifica
integer x = 3365
integer y = 1492
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_palletencab_rotulamodifica
integer x = 3369
integer y = 1664
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_palletencab_rotulamodifica
integer x = 3365
integer y = 80
end type

event pb_buscar::clicked;//
end event

type dw_histofruta from datawindow within w_maed_palletencab_rotulamodifica
boolean visible = false
integer x = 137
integer y = 1916
integer width = 352
integer height = 220
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_palletfrutahisto_proceso"
borderstyle borderstyle = stylelowered!
end type

type dw_histoencab from datawindow within w_maed_palletencab_rotulamodifica
boolean visible = false
integer x = 507
integer y = 1924
integer width = 352
integer height = 220
integer taborder = 100
string title = "none"
string dataobject = "dw_mant_palletencabhisto_proceso"
borderstyle borderstyle = stylelowered!
end type

