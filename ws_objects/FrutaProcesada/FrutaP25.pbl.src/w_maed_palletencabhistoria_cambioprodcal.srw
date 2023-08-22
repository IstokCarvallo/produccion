$PBExportHeader$w_maed_palletencabhistoria_cambioprodcal.srw
forward
global type w_maed_palletencabhistoria_cambioprodcal from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_palletencabhistoria_cambioprodcal
end type
type ids_palletencabhisto from datawindow within w_maed_palletencabhistoria_cambioprodcal
end type
type ids_palletfrutahisto from datawindow within w_maed_palletencabhistoria_cambioprodcal
end type
end forward

shared variables

end variables

global type w_maed_palletencabhistoria_cambioprodcal from w_mant_encab_deta_csd
integer width = 3538
integer height = 2288
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
event ue_imprimir ( )
dw_3 dw_3
ids_palletencabhisto ids_palletencabhisto
ids_palletfrutahisto ids_palletfrutahisto
end type
global w_maed_palletencabhistoria_cambioprodcal w_maed_palletencabhistoria_cambioprodcal

type variables
Integer	ii_yaexiste, il_tipopal = 1
Long 		ia_palletfruta[], il_proceso, ll_cajaspallet


Str_mant 	istr_mant2

w_mant_deta_palletfruta_cambioprodcal iw_mantencion

DataWindowChild	dw_especie, dw_etiqueta, dw_planta,&
                  dw_cliente,dw_condiciones,dw_emba,idwc_categorias,idwc_status,&
						idwc_condicion,idwc_tratamiento,idwc_tipofrio,idwc_destino
						
//DataStore ids_palletencabhisto
//DataStore ids_palletfrutahisto
end variables

forward prototypes
public function boolean existecondicion (integer as_valor)
public function boolean existevariedad (string ls_columna)
public function boolean existeplanta (integer as_valor)
public function boolean existevariecalibre (integer as_valor)
public subroutine buscavariedad ()
public subroutine habilitaencab (boolean habilita)
public subroutine habilitaingreso ()
public subroutine deshabilitaencabezado ()
public function boolean noexistetarja (integer cliente, integer planta, long folio, long pallet)
public subroutine grabahistoria (integer cliente, integer planta, long numero)
public subroutine cuentacajas ()
public function boolean duplicado (integer cliente, integer planta, long numero)
public function string embalajecliente (integer ai_cliente)
public function boolean existetipoembalaje (string as_codigo)
public function boolean existepallet (string ls_columna)
public subroutine buscaembalaje ()
public function boolean existeembalaje (string ls_columna)
protected function boolean wf_actualiza_db (boolean borrando)
protected subroutine grabahistoria_store (integer ai_histo)
public function long buscanuevofolio (integer cliente, integer planta)
public function boolean cliente_rotulado (integer cliente)
end prototypes

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "RECEPCION DE PALLETS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

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

dw_planta.Retrieve(1)

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
 
 SELECT dbo.CONDICION.COND_NOMBRE  
    INTO :ls_descondicion  
    FROM dbo.CONDICION  
   WHERE dbo.CONDICION.COND_CODIGO = : li_condicion   ;

IF sqlca.SQLCode = 0 THEN
	istr_mant.argumento[15]	=	ls_descondicion
ELSE
	istr_mant.argumento[15]	=	""
END IF
RETURN TRUE
end function

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

public function boolean existeplanta (integer as_valor); integer li_planta, li_cliente
 string	ls_desplanta
 
 li_planta	=	as_valor
 li_cliente	= Integer(istr_mant.argumento[1])	
 
 SELECT dbo.PLANTADESP.PLDE_NOMBRE  
    INTO : ls_desplanta  
    FROM  dbo.PLANTADESP  
   WHERE ( dbo.PLANTADESP.PLDE_CODIGO = : li_planta )   ;
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
	WHERE	espe_codigo		= : li_especie  and &
			vari_codigo    = : li_variedad;
			//and seca_codigo		= : ls_secacod ;
IF (sqlca.SQLCode) = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Variecalibre")
	RETURN False
	
ELSEIF Registros < 1 THEN
	MessageBox("Atención","Código de Variedad-Calibre no Existe.", Exclamation!, Ok!)
	RETURN False
END IF
RETURN TRUE
end function

public subroutine buscavariedad ();Str_busqueda	lstr_busq

dw_2.Modify("buscavariedad.border = 0")
dw_2.Modify("buscavariedad.border = 5")

lstr_busq.argum[1]	=	istr_mant.Argumento[1]
lstr_busq.argum[2]	=	String(dw_2.GetItemNumber(1, "espe_codigo"))

OpenWithParm(w_busc_variedades, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[4] <> '' THEN
	istr_mant.argumento[4]	=	lstr_busq.argum[4]
	istr_mant.argumento[5]	=	lstr_busq.argum[5]
	
	dw_2.setItem(1, "vari_codigo", Integer(lstr_busq.argum[4]))
	dw_2.setItem(1, "vari_nombre", lstr_busq.argum[5])
	dw_2.SetColumn("vari_codigo")
	dw_2.SetFocus()
END IF

dw_2.Modify("buscavariedad.border = 0")
dw_2.Modify("buscavariedad.border = 6")
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_2.Object.clie_codigo.Protect	=	0
	dw_2.Object.paen_numero.Protect	=	0
	dw_2.Object.plde_codigo.Protect	=	0
	dw_2.Object.clie_codigo.BackGround.Color = RGB(255,255,255)
	dw_2.Object.paen_numero.BackGround.Color = RGB(255,255,255)
	dw_2.Object.plde_codigo.BackGround.Color = RGB(255,255,255)
	dw_2.SetColumn("clie_codigo")
	dw_2.SetFocus()
ELSE
	dw_2.Object.clie_codigo.Protect	=	1
	dw_2.Object.paen_numero.Protect	=	1
	dw_2.Object.plde_codigo.Protect	=	1
	dw_2.Object.clie_codigo.BackGround.Color = RGB(166,180,210)
	dw_2.Object.paen_numero.BackGround.Color = RGB(166,180,210)
	dw_2.Object.plde_codigo.BackGround.Color = RGB(166,180,210)
	
	IF dw_2.Object.paen_pmixto[1]	=	1 AND Integer(dw_2.Object.paen_pmixto.Protect)	=	1 THEN
		dw_2.Object.vari_codigo.Protect	=	1
		dw_2.Object.emba_codigo.Protect	=	1
		dw_2.Object.vari_codigo.BackGround.Color = RGB(166,180,210)
		dw_2.Object.emba_codigo.BackGround.Color = RGB(166,180,210)
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
	IsNull(dw_2.Object.frio_codigo[1]) OR dw_2.Object.frio_codigo[1] = ""  THEN
	lb_estado = False
ELSEIF dw_2.Object.paen_tipopa[1] = 1 AND &
	IsNull(dw_2.Object.tpem_codigo[1]) OR dw_2.Object.tpem_codigo[1] = '' THEN
	lb_estado = False
END IF

IF	IsNull(dw_2.Object.dest_codigo[1]) OR dw_2.Object.dest_codigo[1] = 0 THEN
	lb_estado = False
END IF


//pb_ins_det.Enabled = lb_estado
end subroutine

public subroutine deshabilitaencabezado ();dw_2.Object.clie_codigo.Protect	=	1
dw_2.Object.plde_codigo.Protect	=	1
dw_2.Object.paen_pmixto.Protect	=	1
dw_2.Object.emba_codigo.Protect	=	1
dw_2.Object.espe_codigo.Protect	=	1
dw_2.Object.vari_codigo.Protect	=	1
dw_2.Object.cond_codigo.Protect	=	1
//dw_2.Object.stat_codigo.Protect	=	1
dw_2.Object.paen_fecemb.Protect	=	1
dw_2.Object.paen_cosecha.Protect	=	1
//dw_2.Object.cate_codigo.Protect	=	1
dw_2.Object.etiq_codigo.Protect	=	1
dw_2.Object.paen_inspec.Protect	=	1
dw_2.Object.paen_estado.Protect	=	1
dw_2.Object.paen_concal.Protect	=	1
dw_2.Object.trat_codigo.Protect	=	1
dw_2.Object.frio_codigo.Protect	=	1
dw_2.Object.dest_codigo.Protect	=	1
dw_2.Object.paen_pmixto.Protect	=	1
dw_2.Object.paen_pexpor.Protect	=	1
//dw_2.Object.paen_nrasda.Protect	=	1
//dw_2.Object.copa_codigo.Protect	=	1

IF il_tipopal = 1 THEN
	dw_2.Object.tpem_codigo.Protect	=	0
	dw_2.Object.paen_ccajas.Protect	=	0
	dw_2.Object.paen_altura.Protect	=	0
	dw_2.Object.paen_altura.Color = 0
	dw_2.Object.tpem_codigo.Color = 0
	dw_2.Object.paen_ccajas.Color = 0
	dw_2.Object.paen_altura.BackGround.Color = RGB(255,255,255)
	dw_2.Object.tpem_codigo.BackGround.Color = RGB(255,255,255)
	dw_2.Object.paen_ccajas.BackGround.Color = RGB(255,255,255)
ELSE
	dw_2.Object.tpem_codigo.Protect	=	1
	dw_2.Object.paen_ccajas.Protect	=	1
	dw_2.Object.paen_altura.Protect	=	1
	dw_2.Object.paen_altura.Color = RGB(255,255,255)
	dw_2.Object.tpem_codigo.Color = RGB(255,255,255)
	dw_2.Object.paen_ccajas.Color = RGB(255,255,255)
	dw_2.Object.paen_altura.BackGround.Color = 553648127
	dw_2.Object.tpem_codigo.BackGround.Color = 553648127
	dw_2.Object.paen_ccajas.BackGround.Color = 553648127
END IF

dw_2.Object.clie_codigo.Color = RGB(255,255,255)
dw_2.Object.plde_codigo.Color = RGB(255,255,255)
dw_2.Object.paen_pmixto.Color = RGB(255,255,255)
dw_2.Object.emba_codigo.Color = RGB(255,255,255)
dw_2.Object.espe_codigo.Color = RGB(255,255,255)
dw_2.Object.vari_codigo.Color = RGB(255,255,255)
dw_2.Object.cond_codigo.Color = RGB(255,255,255)
dw_2.Object.paen_fecemb.Color = RGB(255,255,255)
dw_2.Object.paen_cosecha.Color = RGB(255,255,255)
dw_2.Object.etiq_codigo.Color = RGB(255,255,255)
dw_2.Object.paen_inspec.Color = RGB(255,255,255)
dw_2.Object.paen_estado.Color = RGB(255,255,255)
dw_2.Object.paen_concal.Color = RGB(255,255,255)
dw_2.Object.trat_codigo.Color = RGB(255,255,255)
dw_2.Object.frio_codigo.Color = RGB(255,255,255)
dw_2.Object.dest_codigo.Color = RGB(255,255,255)
dw_2.Object.paen_pmixto.Color = RGB(255,255,255)
dw_2.Object.paen_pexpor.Color = RGB(255,255,255)

dw_2.Object.clie_codigo.BackGround.Color = 553648127
dw_2.Object.plde_codigo.BackGround.Color = 553648127
dw_2.Object.paen_pmixto.BackGround.Color = 553648127
dw_2.Object.emba_codigo.BackGround.Color = 553648127
dw_2.Object.espe_codigo.BackGround.Color = 553648127
dw_2.Object.vari_codigo.BackGround.Color = 553648127
dw_2.Object.cond_codigo.BackGround.Color = 553648127
//dw_2.Object.stat_codigo.BackGround.Color = 553648127
dw_2.Object.paen_fecemb.BackGround.Color = 553648127
dw_2.Object.paen_cosecha.BackGround.Color = 553648127
//dw_2.Object.cate_codigo.BackGround.Color = 553648127
dw_2.Object.etiq_codigo.BackGround.Color = 553648127
dw_2.Object.paen_inspec.BackGround.Color = 553648127
dw_2.Object.paen_estado.BackGround.Color = 553648127
dw_2.Object.paen_concal.BackGround.Color = 553648127
dw_2.Object.trat_codigo.BackGround.Color = 553648127
dw_2.Object.frio_codigo.BackGround.Color = 553648127
dw_2.Object.dest_codigo.BackGround.Color = 553648127
dw_2.Object.paen_pmixto.BackGround.Color = 553648127
dw_2.Object.paen_pexpor.BackGround.Color = 553648127
//dw_2.Object.paen_nrasda.BackGround.Color = 553648127
//dw_2.Object.copa_codigo.BackGround.Color = 553648127

end subroutine

public function boolean noexistetarja (integer cliente, integer planta, long folio, long pallet);Long	registros
  
SELECT	Count(*)  
	INTO	:registros
  	FROM	dbo.alpalletfruta
  	WHERE plde_codigo	=	:planta
  	AND	altu_numero	=	:folio
	AND  	clie_codigo	=	:cliente
	AND 	paen_numero	=	:pallet ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla alpalletfruta")
	RETURN FALSE
ELSEIF registros > 0 THEN
	MessageBox("Atención","Nro de Pallet ya ingresado para el Nro. de Folio", Exclamation!, Ok!)
	RETURN FALSE
END IF

RETURN TRUE
end function

public subroutine grabahistoria (integer cliente, integer planta, long numero);Long		ll_cantidad

SELECT COUNT(*)
INTO  :ll_cantidad
FROM dbo.palletencabhisto
WHERE clie_codigo = :cliente
AND   plde_codigo = :planta
AND   paen_numero = :numero;

IF ll_cantidad = 0 OR IsNull(ll_cantidad) THEN

	INSERT INTO dbo.palletencabhisto (clie_codigo,paen_numero,plde_codigo,pahi_tipopa,
		tpem_codigo,espe_codigo,vari_codigo,tiem_codigo,emba_codigo,cate_codigo,etiq_codigo,
		stat_codigo,trat_codigo,frio_codigo,cond_codigo,dest_codigo,pahi_fecemb,pahi_cosecha,
		paen_altura,paen_ccajas,tmvp_codigo,paen_fecini,paen_horain,cama_codigo,pahi_calle,
		pahi_base,pahi_posici,pahi_estado,pahi_inspec,pahi_concal,pahi_pexpor,pahi_pmixto)
	SELECT clie_codigo,paen_numero,plde_codigo,paen_tipopa,
		tpem_codigo,espe_codigo,vari_codigo,tiem_codigo,emba_codigo,cate_codigo,etiq_codigo,
		stat_codigo,trat_codigo,frio_codigo,cond_codigo,dest_codigo,paen_fecemb,paen_cosecha,
		paen_altura,paen_ccajas,tmvp_codigo,paen_fecini,paen_horain,cama_codigo,paen_calle,
		paen_base,paen_posici,paen_estado,paen_inspec,paen_concal,paen_pexpor,paen_pmixto
	FROM dbo.palletencab
	WHERE clie_codigo = :cliente
	AND   plde_codigo = :planta
	AND   paen_numero = :numero;
	COMMIT;

END IF
	
SELECT COUNT(*)
INTO  :ll_cantidad
FROM dbo.palletfrutahisto
WHERE clie_codigo = :cliente
AND   plde_codigo = :planta
AND   paen_numero = :numero;

IF ll_cantidad = 0 OR IsNull(ll_cantidad) THEN

	INSERT INTO dbo.palletfrutahisto (clie_codigo,paen_numero,espe_codigo,vari_codigo,
		emba_codigo,prod_codigo,cond_codigo,etiq_codigo,plde_codigo,pafh_calibr,
		pafh_secuen,pafh_ccajas,pafh_nrlote,pafr_docrel,pafr_varrot,pafr_huert1,pafr_huert4,
		pafr_cuart1,pafr_cuart4,pafr_prdrot,pafr_calrot,pafr_rotpak,cate_codigo,pafr_catrot)
	SELECT clie_codigo,paen_numero,espe_codigo,vari_codigo,
		emba_codigo,prod_codigo,cond_codigo,etiq_codigo,plde_codigo,pafr_calibr,
		pafr_secuen,pafr_ccajas,pafr_nrlote,pafr_docrel,pafr_varrot,pafr_huert1,pafr_huert4,
		pafr_cuart1,pafr_cuart4,pafr_prdrot,pafr_calrot,pafr_rotpak,cate_codigo,pafr_catrot
	FROM dbo.palletfruta
	WHERE clie_codigo = :cliente
	AND   plde_codigo = :planta
	AND   paen_numero = :numero;
   COMMIT;
	
END IF
end subroutine

public subroutine cuentacajas ();Long	I
istr_mant.Argumento[11]	= string(dw_2.GetItemNumber(1, "paen_ccajas"))
For I=1 to dw_1.RowCount()
	istr_mant.argumento[11]     =	String(Long(istr_mant.argumento[11]) - dw_1.GetitemNumber(I,"pafr_ccajas"))
Next
IF	Long(istr_mant.argumento[11])<0 THEN  istr_mant.argumento[11]='0'

RETURN
end subroutine

public function boolean duplicado (integer cliente, integer planta, long numero);Long		ll_fila
String	ls_client, ls_nropal, ls_planta
	
ls_client = String(cliente)
ls_nropal = String(numero)
ls_planta = String(planta)

ll_fila	= dw_3.Find( "clie_codigo = " + ls_client + &
						    " AND paen_numero = " + ls_nropal + &
							 " AND plde_codigo = " + ls_planta, 1, dw_3.RowCount())

IF ll_fila > 0 THEN
//	MessageBox("Error","Número ya fue Ingresado Anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF


end function

public function string embalajecliente (integer ai_cliente);String	ls_Embalaje


SELECT	Min(emba_codigo)
	INTO	:ls_Embalaje
	FROM	dbo.tipopallemba
	WHERE	clie_codigo	=	:ai_Cliente;

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

//IF dw_2.Object.paen_tipopa[1]=2 THEN RETURN TRUE

SELECT	tpem_cancaj,TPEM_ALTURA
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

public function boolean existepallet (string ls_columna);Integer	li_cliente, li_cantid, li_planta, li_estado
Long		ll_nropal, ll_pcopda
Boolean	lb_retorno = False

li_cliente					=	Integer(dw_2.GetItemNumber(1, "clie_codigo"))
li_planta					=	Integer(dw_2.GetItemNumber(1, "plde_codigo"))
ll_nropal 					=	Long(ls_columna)

istr_mant.argumento[2]	=	String(ll_nropal)

SELECT	Count(*)
	INTO	:li_cantid
	FROM	dbo.palletencab
	WHERE	clie_codigo		= 	:li_cliente
	AND	paen_numero    = 	:ll_nropal
	AND	plde_codigo		=	:li_planta;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
	lb_retorno = True
ELSEIF li_cantid = 0 THEN
		MessageBox("Atención","Pallet NO Pertenece a Cliente o Planta", Exclamation!, Ok!)
		lb_retorno = True	
ELSEIF li_cantid > 0 THEN
	
	SELECT 	paen_pcopda,paen_estado
		INTO	:ll_pcopda,:li_estado
		FROM	dbo.palletencab
		WHERE	clie_codigo	= 	:li_cliente
		AND	paen_numero = 	:ll_nropal
		AND	plde_codigo	=	:li_planta;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla palletencab")
		lb_retorno = True
//	ELSEIF ll_pcopda > 1 THEN	
//			MessageBox("Atención","El Numero de Pallet Ingresado es un Ingreso Caja a Caja. Ingrese Otro.", Exclamation!, Ok!)
//			lb_retorno = True		
	ELSE	

		IF ll_pcopda > 1 THEN 
			MessageBox("Atención","El Numero de Pallet Ingresado es un Ingreso Caja a Caja.", Exclamation!, Ok!)
			dw_1.Enabled	=	False
		END IF
		
		IF li_estado <> 1 THEN 
			MessageBox("Atención","El Numero de Pallet Fue Despachado.", Exclamation!, Ok!)
			dw_1.Enabled	=	False
			lb_retorno = True	
		END IF
	
		IF Not lb_retorno THEN 
			This.TriggerEvent("ue_recuperadatos")

			dw_emba.Retrieve(li_cliente, dw_2.Object.emba_codigo[1])
	
			istr_mant.argumento[3] = String(dw_2.Object.espe_codigo[1])
			istr_mant.argumento[4] = String(dw_2.Object.vari_codigo[1])
			istr_mant.argumento[5] = dw_2.Object.vari_nombre[1]
			istr_mant.argumento[6] = String(dw_2.Object.plde_codigo[1])
			istr_mant.argumento[7] = dw_2.Object.emba_codigo[1]
			istr_mant.argumento[8] = dw_2.Object.emba_nombre[1]
			istr_mant.argumento[9] = String(dw_2.Object.etiq_codigo[1])
			istr_mant.argumento[10] = String(dw_2.Object.cond_codigo[1])
			istr_mant.argumento[12] = ""
			istr_mant.argumento[40] = String(dw_2.Object.paen_fecemb[1])
			
			IF dw_2.Object.paen_tipopa[1] = 1 THEN
				ExisteTipoEmbalaje(String(dw_2.Object.tpem_codigo[1]))
			END IF
			
			ExistePlanta(dw_2.Object.plde_codigo[1])
			//ExistePlanta(gi_codplanta)
			Existecondicion(dw_2.Object.cond_codigo[1])
		End If
	END IF
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
	F_errorbasedatos(sqlca,"Lectura tabla Embalajes")
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

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno
long ll_fila, pallet
integer cliente,planta

DwItemStatus	Estadol
FOR ll_fila = 1 TO ids_palletencabhisto.RowCount()		 
	Estadol	= ids_palletencabhisto.GetItemStatus(ll_fila, 0, Primary!)
	cliente  = ids_palletencabhisto.getitemnumber(1,"clie_codigo")
	planta   = ids_palletencabhisto.getitemnumber(1,"plde_codigo")
	pallet   = ids_palletencabhisto.getitemnumber(1,"paen_numero")
NEXT

FOR ll_fila = 1 TO ids_palletfrutahisto.RowCount()		 
	Estadol	= ids_palletfrutahisto.GetItemStatus(ll_fila, 0, Primary!)
	cliente  = ids_palletfrutahisto.getitemnumber(ll_fila,"clie_codigo")
	planta   = ids_palletfrutahisto.getitemnumber(ll_fila,"plde_codigo")
	pallet   = ids_palletfrutahisto.getitemnumber(ll_fila,"paen_numero")
	
NEXT

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
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF ids_palletencabhisto.Update() = 1 THEN
				IF ids_palletfrutahisto.Update() = 1 THEN 
			
					Commit;
					
					IF sqlca.SQLCode <> 0 THEN
						F_ErrorBaseDatos(sqlca, This.Title)
						
						RollBack;
					ELSE
						lb_Retorno	=	True
						
						dw_1.ResetUpdate()
						dw_2.ResetUpdate()
						ids_palletfrutahisto.ResetUpdate()
						ids_palletencabhisto.ResetUpdate()
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
			
			RollBack;
		END IF
	ELSE
		F_ErrorBaseDatos(sqlca, This.Title)
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

protected subroutine grabahistoria_store (integer ai_histo);Long ll_cantidad, ll_numero, ll_fila, ll_fila_d
Integer	li_cliente, li_planta

li_cliente	=	dw_2.Object.clie_codigo[1]
li_planta	=	dw_2.Object.plde_codigo[1]
ll_numero	=	dw_2.Object.paen_numero[1]

IF ai_histo = 0 THEN  
	/* Encabezado*/
//	SELECT COUNT(*)
//	INTO  :ll_cantidad
//	FROM dbo.palletencabhisto
//	WHERE clie_codigo = :li_cliente
//	AND   plde_codigo = :li_planta
//	AND   paen_numero = :ll_numero;
//	
//	IF ll_cantidad = 0 OR IsNull(ll_cantidad) THEN

		ll_fila	= ids_palletencabhisto.InsertRow(0)
		IF ll_fila > 0 THEN
			ids_palletencabhisto.SetItem(ll_fila,"clie_codigo",dw_2.Object.clie_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"paen_numero",dw_2.Object.paen_numero[1])
			ids_palletencabhisto.SetItem(ll_fila,"plde_codigo",dw_2.Object.plde_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_tipopa",dw_2.Object.paen_tipopa[1])
			//ids_palletencabhisto.SetItem(ll_fila,"tpem_codigo",istr_mant.Argumento[29])
			ids_palletencabhisto.SetItem(ll_fila,"tpem_codigo",dw_2.Object.tpem_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"espe_codigo",dw_2.Object.espe_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"vari_codigo",dw_2.Object.vari_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"tiem_codigo",dw_2.Object.tiem_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"emba_codigo",dw_2.Object.emba_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"cate_codigo",dw_2.Object.cate_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"etiq_codigo",dw_2.Object.etiq_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"stat_codigo",dw_2.Object.stat_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"trat_codigo",dw_2.Object.trat_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"frio_codigo",dw_2.Object.frio_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"cond_codigo",dw_2.Object.cond_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"dest_codigo",dw_2.Object.dest_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_fecemb",dw_2.Object.paen_fecemb[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_cosecha",dw_2.Object.paen_cosecha[1])
			ids_palletencabhisto.SetItem(ll_fila,"paen_altura",dw_2.Object.paen_altura[1])
			//ids_palletencabhisto.SetItem(ll_fila,"paen_ccajas",Long(istr_mant.Argumento[30]))
			ids_palletencabhisto.SetItem(ll_fila,"paen_ccajas",dw_2.Object.paen_ccajas[1])
			ids_palletencabhisto.SetItem(ll_fila,"tmvp_codigo",dw_2.Object.tmvp_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"paen_fecini",dw_2.Object.paen_fecini[1])
			ids_palletencabhisto.SetItem(ll_fila,"paen_horain",dw_2.Object.paen_horain[1])
			ids_palletencabhisto.SetItem(ll_fila,"cama_codigo",dw_2.Object.cama_codigo[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_calle",dw_2.Object.paen_calle[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_base",dw_2.Object.paen_base[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_posici",dw_2.Object.paen_posici[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_estado",dw_2.Object.paen_estado[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_inspec",dw_2.Object.paen_inspec[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_concal",dw_2.Object.paen_concal[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_pexpor",dw_2.Object.paen_pexpor[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_pmixto",dw_2.Object.paen_pmixto[1])
			ids_palletencabhisto.SetItem(ll_fila,"pahi_proces",Long(istr_mant.argumento[45]))
			
		END IF
	//END IF
ELSE			
	/* Detalle*/
//	SELECT COUNT(*)
//	INTO  :ll_cantidad
//	FROM dbo.palletfrutahisto
//	WHERE clie_codigo = :li_cliente
//	AND   plde_codigo = :li_planta
//	AND   paen_numero = :ll_numero;
//
//	IF ll_cantidad = 0 OR IsNull(ll_cantidad) THEN	
		FOR ll_fila_d = 1 TO dw_1.RowCount()	
			ll_fila	= 	ids_palletfrutahisto.InsertRow(0)   
			IF ll_fila > 0 THEN	
				ids_palletfrutahisto.SetItem(ll_fila,"clie_codigo",dw_1.Object.clie_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"paen_numero",dw_1.Object.paen_numero[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"espe_codigo",dw_1.Object.espe_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"vari_codigo",dw_1.Object.vari_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"emba_codigo",dw_1.Object.emba_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"prod_codigo",dw_1.Object.prod_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"cond_codigo",dw_1.Object.cond_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"etiq_codigo",dw_1.Object.etiq_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"plde_codigo",dw_1.Object.plde_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafh_calibr",dw_1.Object.pafr_calibr[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafh_secuen",dw_1.Object.pafr_secuen[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafh_ccajas",dw_1.Object.pafr_ccajas[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafh_nrlote",dw_1.Object.pafr_nrlote[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafh_proces",Long(istr_mant.argumento[45]))
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_fecing",dw_1.Object.pafr_fecing[ll_fila_d])
			   ids_palletfrutahisto.SetItem(ll_fila,"pafr_fecemb",dw_1.Object.pafr_fecemb[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_tipdoc",13)
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_copack",dw_1.Object.pafr_copack[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_varrot",dw_1.Object.pafr_varrot[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_huert1",dw_1.Object.pafr_huert1[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_huert4",dw_1.Object.pafr_huert4[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_cuart1",dw_1.Object.pafr_huert1[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_cuart4",dw_1.Object.pafr_huert1[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_prdrot",dw_1.Object.pafr_prdrot[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_calrot",dw_1.Object.pafr_calrot[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_rotpak",dw_1.Object.pafr_rotpak[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"cate_codigo",dw_1.Object.cate_codigo[ll_fila_d])
				ids_palletfrutahisto.SetItem(ll_fila,"pafr_catrot",dw_1.Object.pafr_catrot[ll_fila_d])
			END IF
		NEXT
	//END IF
END IF

RETURN
end subroutine

public function long buscanuevofolio (integer cliente, integer planta);Integer	li_planta, li_cliente
Long		ll_numero

li_cliente	=	cliente	
li_planta	=	planta


SELECT max(pahi_proces) INTO:ll_numero
FROM dbo.palletencabhisto
WHERE	clie_codigo = :li_cliente
AND	plde_codigo = :li_planta
AND	pahi_proces < 99999990;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Alpalletencab")
ELSEIF IsNull(ll_numero) THEN
	ll_numero=1
ELSEIF ll_numero=0 THEN
	ll_numero=1
ELSE
	ll_numero++	
END IF


RETURN ll_numero
end function

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

event open;Integer	li_cliente, li_planta
String	ls_Embalaje

X	=	10
Y	=	300
This.Height	=	2020
im_menu		=	m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

buscar	= "Código:Ncodigo,Descripción:Sconcepto"
ordenar	= "Código:codigo,Descripción:concepto"

istr_mant2						=	Message.PowerObjectParm
istr_mant                  =	Message.PowerObjectParm

li_cliente					=	Integer(istr_mant2.Argumento[3])
li_planta					=	Integer(istr_mant2.Argumento[1])
ls_Embalaje				   =	EmbalajeCliente(li_Cliente)

IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

istr_mant.solo_consulta	=	False
istr_mant.argumento[14]	=	''
istr_mant.argumento[16]	=	''

dw_2.GetChild("clie_codigo", dw_cliente)
dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("espe_codigo", dw_especie)
dw_2.GetChild("etiq_codigo", dw_etiqueta)
dw_2.GetChild("tpem_codigo", dw_emba)
dw_2.GetChild("cate_codigo", idwc_categorias)
dw_2.GetChild("stat_codigo", idwc_status)
dw_2.GetChild("cond_codigo", idwc_condicion)
dw_2.GetChild("trat_codigo", idwc_tratamiento)
dw_2.GetChild("frio_codigo", idwc_tipofrio)
dw_2.GetChild("dest_codigo", idwc_destino)

dw_cliente.SetTransObject(sqlca)
dw_planta.SetTransObject(sqlca)
dw_especie.SetTransObject(sqlca)
dw_etiqueta.SetTransObject(sqlca)
dw_emba.SetTransObject(sqlca)
idwc_categorias.SetTransObject(sqlca)
idwc_status.SetTransObject(sqlca)
idwc_condicion.SetTransObject(sqlca)
idwc_tratamiento.SetTransObject(sqlca)
idwc_tipofrio.SetTransObject(sqlca)
idwc_destino.SetTransObject(sqlca)

dw_cliente.Retrieve()
dw_planta.Retrieve(1)
dw_especie.Retrieve()
dw_etiqueta.Retrieve()
dw_emba.Retrieve(li_cliente, ls_Embalaje)
idwc_categorias.Retrieve()
idwc_status.Retrieve()
idwc_condicion.Retrieve()
idwc_tratamiento.Retrieve()
idwc_tipofrio.Retrieve()
idwc_destino.Retrieve()

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()

pb_nuevo.PostEvent(Clicked!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

//// Para actualizar en el historico
//ids_palletencabhisto 				= Create DataStore 
//ids_palletfrutahisto					= Create DataStore
//
//ids_palletencabhisto.DataObject 	= "dw_mant_palletencabhisto"
//ids_palletfrutahisto.DataObject 	= "dw_mant_palletfrutahisto"
ids_palletencabhisto.SetTransObject(sqlca)
ids_palletfrutahisto.SetTransObject(sqlca)

istr_mant.argumento[1]	=	String(li_cliente)
istr_mant.argumento[6]	=	String(li_planta)
istr_mant.argumento[7]	=	ls_Embalaje
istr_mant.argumento[22]	=	'1'		//	Pallet Exportación
istr_mant.argumento[23]	=	'0'		//	Pallet Mixto

dw_2.Object.paen_altura.Protect	=	0
dw_2.Object.tpem_codigo.Protect	=	0
dw_2.Object.paen_ccajas.Protect	=	0
dw_2.Object.copa_codigo.Protect	=	0
dw_2.Object.copa_codigo.BackGround.Color = RGB(255,255,255)
dw_2.Object.paen_altura.BackGround.Color = RGB(255,255,255)
dw_2.Object.tpem_codigo.BackGround.Color = RGB(255,255,255)
dw_2.Object.paen_ccajas.BackGround.Color = RGB(255,255,255)

dw_3.DataObject			=	istr_mant2.Argumento[5]
dw_3.SetTransObject(sqlca)
istr_mant2.dw.ShareData(dw_3)


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

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)


end event

event ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, ll_fila_f, ll_nuevofolio, ll_cajas

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	
	ids_palletencabhisto.Reset()
	ids_palletfrutahisto.Reset()			
	
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]), &
							Integer(istr_mant.argumento[6]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos 1.", &
										Information!, RetryCancel!)
	ELSE
		ll_cajas = dw_2.Object.paen_ccajas[1]
		
		istr_mant.argumento[7] = dw_2.Object.emba_codigo[1]
			
		dw_2.GetChild("tpem_codigo", dw_emba)
		dw_emba.SetTransObject(sqlca)
		dw_emba.Retrieve(Integer(istr_mant.argumento[1]),istr_mant.argumento[7])
		
		il_tipopal = dw_2.Object.paen_tipopa[1]
			
		IF dw_2.Object.paen_tipopa[1] = 1 THEN
			dw_2.Object.paen_altura.Protect	=	0
			dw_2.Object.tpem_codigo.Protect	=	0
			dw_2.Object.paen_ccajas.Protect	=	0
			
			dw_2.Object.paen_altura.Color = 0
			dw_2.Object.tpem_codigo.Color = 0
			dw_2.Object.paen_ccajas.Color = 0
			
			dw_2.Object.paen_altura.BackGround.Color = RGB(255,255,255)
			dw_2.Object.tpem_codigo.BackGround.Color = RGB(255,255,255)
			dw_2.Object.paen_ccajas.BackGround.Color = RGB(255,255,255)
		ELSE	
			
			dw_2.Object.paen_altura.Protect	=	1
			dw_2.Object.tpem_codigo.Protect	=	1
			dw_2.Object.paen_ccajas.Protect	=	1
			dw_2.Object.paen_altura.Color = RGB(255,255,255)
			dw_2.Object.tpem_codigo.Color = RGB(255,255,255)
			dw_2.Object.paen_ccajas.Color = RGB(255,255,255)
			dw_2.Object.paen_altura.BackGround.Color = RGB(166,180,210)
			dw_2.Object.tpem_codigo.BackGround.Color = RGB(166,180,210)
			dw_2.Object.paen_ccajas.BackGround.Color = RGB(166,180,210)
		END IF		
				
		IF Integer(istr_mant.argumento[46]) = 1 THEN
			dw_emba.SetFilter("tpem_cancaj < "+ String(ll_cajas) )
			dw_emba.Filter()
			
		ElSEIF Integer(istr_mant.argumento[46]) = 2 THEN
			dw_emba.SetFilter("tpem_cancaj > "+ String(ll_cajas))
			dw_emba.Filter()		
		END IF
		// Genera un nuevo Proceso si este viene e Nulo
		IF IsNull(istr_mant.argumento[45]) OR Long(istr_mant.argumento[45])= 0 THEN
			ll_nuevofolio=buscanuevofolio(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[6]))
			istr_mant.argumento[45] = String(ll_nuevofolio)
		END IF
		
		// para guardar antes de modificar el encabezado total de cajas y tipo pallet
		istr_mant.Argumento[29]	= dw_2.GetItemString(1, "tpem_codigo")
		istr_mant.Argumento[30]	= string(dw_2.GetItemNumber(1, "paen_ccajas"))
		grabahistoria_store(0)		
		
		istr_mant.Argumento[22]	=	String(dw_2.Object.paen_pexpor[1])
		istr_mant.Argumento[23]	=	String(dw_2.Object.paen_pmixto[1])
		
		DO
			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]), &
									Integer(istr_mant.argumento[6]))

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos 2.", &
												Information!, RetryCancel!)
			ELSE
				pb_eliminar.Enabled	= True
				//pb_grabar.Enabled		= True
				pb_imprimir.Enabled	= True
				//pb_ins_det.Enabled	= True

				IF ll_fila_d > 0 THEN
					//pb_eli_det.Enabled = True
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
					// para guardar antes de modificar el detalle
					grabahistoria_store(1)

				ELSE
					//pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_maed_palletencabhistoria_cambioprodcal.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.ids_palletencabhisto=create ids_palletencabhisto
this.ids_palletfrutahisto=create ids_palletfrutahisto
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.ids_palletencabhisto
this.Control[iCurrent+3]=this.ids_palletfrutahisto
end on

on w_maed_palletencabhistoria_cambioprodcal.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.ids_palletencabhisto)
destroy(this.ids_palletfrutahisto)
end on

event ue_nuevo;call super::ue_nuevo;HabilitaEncab(True)

dw_2.SetItem(1, "clie_codigo", Integer(istr_mant.Argumento[1]))
dw_2.SetItem(1, "plde_codigo", Integer(istr_mant.Argumento[6]))
dw_2.SetItem(1,"espe_codigo", gi_CodEspecie)

istr_mant.argumento[3]	=	String(gi_CodEspecie)
istr_mant.argumento[9]	=	"1"
istr_mant.argumento[10]	=	"0"

DeshabilitaEncabezado()

IF Upperbound(istr_mant2.argumento) = 11 THEN
	dw_2.Object.paen_numero[dw_2.GetRow()] = Long(istr_mant2.Argumento[11])
	ExistePallet(istr_mant2.Argumento[11])
END IF
end event

event ue_seleccion;call super::ue_seleccion;istr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1]) 
istr_busq.argum[2]	=	""

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

event ue_antesguardar;call super::ue_antesguardar;Long		ll_fila, ll_numero, ll_fila2, li_fila3, ll_nuevofolio
Integer	li_Secuencia, li_cliente, li_planta, li_paen_mixto, li_histo=1
li_cliente	=	dw_2.Object.clie_codigo[1]
li_planta	=	dw_2.Object.plde_codigo[1]
ll_numero	=	dw_2.Object.paen_numero[1]

 SELECT	IsNull(Max(pafr_secuen), 0)
	INTO	:li_Secuencia
	FROM	dba.palletfruta
	WHERE	clie_codigo	=	:li_cliente
	AND	plde_codigo =	:li_planta
	AND	paen_numero	=	:ll_numero;

FOR ll_fila = 1 TO dw_1.RowCount()
	
	dw_1.Object.clie_codigo[ll_fila]	=	dw_2.Object.clie_codigo[1]
	dw_1.Object.plde_codigo[ll_fila]	=	dw_2.Object.plde_codigo[1]
	dw_1.Object.paen_numero[ll_fila]	=	dw_2.Object.paen_numero[1]
//	dw_1.Object.espe_codigo[ll_fila]	=	dw_2.Object.espe_codigo[1]
//	dw_1.Object.cond_codigo[ll_fila]	=	dw_2.Object.cond_codigo[1]
//	dw_1.Object.etiq_codigo[ll_fila]	=	dw_2.Object.etiq_codigo[1]

	IF IsNull(dw_2.Object.paen_pmixto[1]) THEN
		li_paen_mixto = 0
   ELSE
		li_paen_mixto = dw_2.Object.paen_pmixto[1]
	END IF 
	
//	IF li_paen_mixto <> 1 THEN
//		dw_1.Object.vari_codigo[ll_fila]	=	dw_2.Object.vari_codigo[1]
//		dw_1.Object.emba_codigo[ll_fila]	=	dw_2.Object.emba_codigo[1]
//	END IF
	
//	IF dw_1.GetItemStatus(ll_Fila, 0, Primary!) = NewModified! THEN
//		li_Secuencia ++
//		dw_1.Object.pafr_secuen[ll_Fila] =	li_Secuencia
//	END IF
	
NEXT

IF NoExisteTarja(Integer(istr_mant.Argumento[1]), Integer(istr_mant2.Argumento[1]), &
						Long(istr_mant2.Argumento[2]), dw_2.Object.paen_numero[1]) THEN

		IF NOT Duplicado(Integer(istr_mant.Argumento[1]), Integer(istr_mant2.Argumento[1]),dw_2.Object.paen_numero[1]) THEN
			IF dw_3.DataObject = 'dw_mues_repalletdeta' THEN
				//dw_3.Reset()
				FOR li_fila3 = 1 to dw_1.RowCount()
					ll_fila2	=	dw_3.InsertRow(0)
					dw_3.Object.plde_codigo[ll_fila2]		=	dw_2.Object.plde_codigo[1]
					dw_3.Object.repe_numero[ll_fila2]	   =	Long(istr_mant2.Argumento[2])
					dw_3.Object.clie_codigo[ll_fila2]		=	dw_2.Object.clie_codigo[1]
					dw_3.Object.paen_numero[ll_fila2]	   =	dw_2.Object.paen_numero[1]
					dw_3.Object.repd_tipood[ll_fila2]		=	li_fila3
					dw_3.Object.paen_tipopa[ll_fila2]		=	Integer(istr_mant2.argumento[46])
					dw_3.Object.vari_nombre[ll_fila2]		=  dw_2.Object.vari_nombre[1]
					//dw_3.Object.emba_codigo[ll_fila2]		=  dw_2.Object.emba_codigo[1]
					dw_3.Object.cate_codigo[ll_fila2]		=  dw_2.Object.cate_codigo[1]
					dw_3.Object.paen_ccajas[ll_fila2]		=  dw_1.Object.pafr_ccajas[li_fila3]
					
					IF dw_3.DataObject = 'dw_mues_repalletdeta' THEN
						dw_3.Object.pafr_secuen[ll_fila2]	=	li_fila3
					END IF	
				NEXT
			ELSE
				ll_fila2	=	dw_3.InsertRow(0)
				dw_3.Object.plde_codigo[ll_fila2]		=	dw_2.Object.plde_codigo[1]
				dw_3.Object.altu_numero[ll_fila2]		=	Long(istr_mant2.Argumento[2])
				dw_3.Object.clie_codigo[ll_fila2]		=	dw_2.Object.clie_codigo[1]
				dw_3.Object.paen_numero[ll_fila2]	   =	dw_2.Object.paen_numero[1]
				dw_3.Object.alpf_fecmov[ll_fila2]		=	Date(istr_mant2.Argumento[9])
			END IF
		END IF
END IF



end event

event ue_modifica_detalle;Cuentacajas()

IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega = False


//	istr_mant.argumento[4]	=	String(dw_1.Object.vari_codigo[il_fila]) 
//	istr_mant.argumento[7]	=	dw_1.Object.emba_codigo[il_fila] 
istr_mant.argumento[17]	=	String(dw_1.Object.prod_codigo[il_fila]) 
istr_mant.argumento[16]	=	String(dw_1.Object.pafr_huert1[il_fila]) 
	
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

DELETE dba.Histcontcalidad
WHERE clie_codigo = :li_Cliente
AND   plde_codigo = :li_Planta
AND   paen_numero = :ll_Numero;

DELETE dba.Recfruproced
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

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
//
end event

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_palletencabhistoria_cambioprodcal
integer x = 55
integer y = 1148
integer width = 3086
integer height = 752
integer taborder = 110
string title = "Detalle de Pallets"
string dataobject = "dw_mues_palletfruta_cambioprodcal"
boolean righttoleft = true
end type

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_palletencabhistoria_cambioprodcal
integer x = 78
integer y = 56
integer width = 2953
integer height = 1048
string dataobject = "dw_mant_palletencab_cambioprodcalstatus"
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null, ll_cancaj
String	ls_columna, ls_embalaje
Date		ld_nula
Integer  li_cliente


DataWIndowChild	dw_calibres

SetNull(ll_null)
SetNull(ld_nula)
ls_columna = GetColumnName()

ii_yaexiste	=	0

dw_2.AcceptText()

CHOOSE CASE ls_columna
	CASE "clie_codigo"
		IF F_ValidaCliente(Integer(data)) THEN
			istr_mant.argumento[1]	= data
			dw_especie.Retrieve(Integer(data))
			dw_etiqueta.Retrieve(Integer(data))
			cliente_rotulado(Integer(data))
			dw_2.GetChild("plde_codigo", dw_planta)
			dw_planta.SetTransObject(sqlca)
			dw_planta.Retrieve(Integer(data), 1)
			
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
		IF ExistePallet(data) OR IsNull(data) THEN
			ii_yaexiste = 1
			dw_2.SetItem(1, "paen_numero", ll_null)
			RETURN 1
		ELSE
			ll_cajaspallet = dw_2.Object.paen_ccajas[1]
			pb_grabar.Enabled = True
		END IF
		
		
	CASE "paen_tipopa"
		
		il_tipopal = Integer(data)
		IF data = '1' THEN
			istr_mant.argumento[7] = dw_2.Object.emba_codigo[1]
			
			dw_2.GetChild("tpem_codigo", dw_emba)
			dw_emba.SetTransObject(sqlca)
			dw_emba.Retrieve(Integer(istr_mant.argumento[1]),istr_mant.argumento[7])
			
			dw_2.Object.paen_altura.Protect	=	0
			dw_2.Object.tpem_codigo.Protect	=	0
			dw_2.Object.paen_ccajas.Protect	=	0
			dw_2.Object.paen_altura.BackGround.Color = RGB(255,255,255)
			dw_2.Object.tpem_codigo.BackGround.Color = RGB(255,255,255)
			dw_2.Object.paen_ccajas.BackGround.Color = RGB(255,255,255)
		ELSE	
			dw_2.SetItem(1, "tpem_codigo", String(ll_null))
			dw_2.Object.paen_altura.Protect	=	1
			dw_2.Object.tpem_codigo.Protect	=	1
			dw_2.Object.paen_ccajas.Protect	=	1
			dw_2.Object.paen_altura.BackGround.Color = RGB(166,180,210)
			dw_2.Object.tpem_codigo.BackGround.Color = RGB(166,180,210)
			dw_2.Object.paen_ccajas.BackGround.Color = RGB(166,180,210)
		END IF		
		
		dw_2.AcceptText()
	
	CASE "espe_codigo"
		istr_mant.argumento[3]	= data
		dw_2.SetItem(1, "vari_nombre", "")
		dw_2.SetItem(1, "vari_codigo", ll_null)
				
	CASE "vari_codigo"
		IF ExisteVariedad(data) = False THEN
			dw_2.SetItem(1, "vari_nombre", "")
			dw_2.SetItem(1, "vari_codigo", ll_null)
			RETURN 1
		END IF
					
	CASE "plde_codigo"
		istr_mant.argumento[6]	= data
		
	CASE "emba_codigo"
		IF ExisteEmbalaje(data) = False THEN
			dw_2.SetItem(1, "emba_nombre", "")
			dw_2.SetItem(1, "emba_codigo", ll_null)
			RETURN 1
		ELSE
		 dw_emba.Retrieve(Integer(istr_mant.argumento[1]),istr_mant.argumento[7])
		END IF
			
	CASE "tpem_codigo"
		li_cliente = Integer(istr_mant.argumento[1])
		ls_embalaje = istr_mant.argumento[7]
		
		SELECT	tpem_cancaj
		INTO	:ll_cancaj
		FROM	dba.TIPOPALLEMBA
		WHERE	clie_codigo	=	:li_cliente
		AND	emba_codigo	=	:ls_embalaje
		AND	tpem_codigo	=	:data;
		
		IF Integer(istr_mant.argumento[46]) = 1 THEN
			IF Integer(ll_cancaj) > ll_cajaspallet THEN
				MessageBox("Atención","Nº cajas no puede ser mayor a lo que tiene. Ingrese Otro.", Exclamation!, Ok!)
				dw_2.SetItem(1, "tpem_codigo", String(ll_null))
			RETURN 1
			END IF	
		ELSEIF	Integer(istr_mant.argumento[46]) = 2 THEN
			IF Integer(ll_cancaj) < ll_cajaspallet THEN
				MessageBox("Atención","Nº cajas no puede ser menor a lo que tiene. Ingrese Otro.", Exclamation!, Ok!)
				dw_2.SetItem(1, "tpem_codigo", String(ll_null))
				RETURN 1
			END IF
		END IF 
		
		IF dw_2.object.paen_tipopa[Row] = 1 THEN
			IF ExisteTipoEmbalaje(data) = False THEN
				dw_2.SetItem(1, "tpem_codigo", String(ll_null))
				RETURN 1
			END IF
		END IF
		
		IF dw_1.RowCount() > 0 THEN
			pb_grabar.Enabled		= True
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
			This.SetItem(Row, ls_Columna, ld_nula)
			RETURN 1
		ELSE
			istr_mant.Argumento[40] = Data
		END IF		

	CASE "paen_cosecha"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, ls_Columna, ld_nula)
			RETURN 1
		END IF				
		

//	CASE "dest_codigo"
//		istr_mant.argumento[33]	= data
//		IF Isnull(data) or data = " " THEN
//			pb_ins_det.Enabled	=	False
//			dw_2.SetItem(1, "dest_codigo", ll_null)
//			RETURN 1
//		END IF

END CHOOSE

DeshabilitaEncabezado()
HabilitaIngreso()
end event

event dw_2::clicked;call super::clicked;IF dw_2.Object.paen_pmixto[1]	=	1 AND Integer(dw_2.Object.paen_pmixto.Protect)	=	1 THEN RETURN

CHOOSE CASE dwo.name
		
	CASE "buscavariedad"
		//buscavariedad()
		
	CASE "buscaembalaje"
		//buscaembalaje()
		
END CHOOSE
end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_palletencabhistoria_cambioprodcal
integer y = 252
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_palletencabhistoria_cambioprodcal
boolean visible = false
integer y = 480
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_palletencabhistoria_cambioprodcal
integer y = 688
boolean enabled = true
end type

event pb_grabar::clicked;IF dw_2.Object.paen_tipopa[1] = 1 THEN
	IF isnull(dw_2.Object.tpem_codigo[1]) OR dw_2.Object.tpem_codigo[1] = ''  THEN
		MessageBox("Error de Consistencia", "Falta el ingreso del Tipo Pallet.", StopSign!, Ok!)
		Message.DoubleParm = -1
		Return
	ELSE
		Parent.TriggerEvent("ue_guardar")
	END IF
ELSE	
	Parent.TriggerEvent("ue_guardar")
END IF	
end event

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_palletencabhistoria_cambioprodcal
boolean visible = false
integer y = 896
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_palletencabhistoria_cambioprodcal
integer y = 1156
end type

event pb_salir::clicked;CloseWithReturn(Parent, istr_mant2)
//
end event

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_palletencabhistoria_cambioprodcal
integer x = 3301
integer y = 1484
integer taborder = 90
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_palletencabhistoria_cambioprodcal
integer x = 3301
integer y = 1660
integer taborder = 100
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_palletencabhistoria_cambioprodcal
boolean visible = false
integer y = 76
boolean enabled = false
end type

type dw_3 from datawindow within w_maed_palletencabhistoria_cambioprodcal
boolean visible = false
integer x = 23
integer y = 1840
integer width = 3081
integer height = 672
integer taborder = 80
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_alpalletfruta"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type ids_palletencabhisto from datawindow within w_maed_palletencabhistoria_cambioprodcal
boolean visible = false
integer x = 174
integer y = 1476
integer width = 791
integer height = 396
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_palletencabhisto"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type ids_palletfrutahisto from datawindow within w_maed_palletencabhistoria_cambioprodcal
boolean visible = false
integer x = 1019
integer y = 1480
integer width = 791
integer height = 396
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_palletfrutahisto"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

