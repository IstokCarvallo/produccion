$PBExportHeader$w_maed_recfruprocee_granel.srw
forward
global type w_maed_recfruprocee_granel from w_mant_encab_deta_csd
end type
end forward

shared variables

end variables

global type w_maed_recfruprocee_granel from w_mant_encab_deta_csd
integer width = 3547
integer height = 2200
string title = "RECEPCION PACKING CENTRAL GRANEL"
string menuname = ""
end type
global w_maed_recfruprocee_granel w_maed_recfruprocee_granel

type variables
Integer	ii_yaexiste, ii_enva_tipoen, ii_enva_codigo, ii_borra=0, &
			ii_Packing

Boolean	ib_existe_folio
//w_maed_palletencab_recepcion_granel  iw_mantencion

DataWindowChild	dw_especie, dw_etiqueta, dw_planta,&
                  dw_cliente,dw_condiciones,dw_emba, dw_ptaori
						
DataStore	ids_Palletfruta
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
protected function boolean wf_actualiza_db (boolean borrando)
public subroutine buscaenvase ()
public subroutine buscaenvasecod (string as_embalaje)
public function long buscanuevofolio (integer ai_cliente, integer ai_planta)
public subroutine cuentatarjas ()
public function boolean existefolio (string as_columna, string as_valor)
public function boolean noexistecliente (integer ai_codigo)
end prototypes

public function boolean existecondicion (integer as_valor);// integer	li_condicion
// String	ls_descondicion
// 
// li_condicion	= as_valor
// 
// SELECT cond_nombre 
//    INTO :ls_descondicion  
//    FROM dba.condicion
//   WHERE cond_codigo = : li_condicion   ;
//
//IF sqlca.SQLCode = 0 THEN
//	istr_mant.argumento[15]	=	ls_descondicion
//ELSE
//	istr_mant.argumento[15]	=	""
//END IF
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
	FROM	dba.variedades
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
FROM  dba.plantadesp
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
	FROM	dba.variecalibre
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
//
//SELECT	Min(emba_codigo)
//	INTO	:ls_Embalaje
//	FROM	dba.tipopallemba
//	WHERE	clie_codigo	=	:ai_Cliente ;
//
//IF sqlca.SQLCode = -1 THEN
//	F_ErrorBaseDatos(sqlca, "Lectura Tipos de Pallet por Embalaje")
//END IF

RETURN ls_Embalaje
end function

public function boolean existetipoembalaje (string as_codigo);//String	ls_embala
//Integer	li_cliente, li_cancaj
//Long		altura
//
//li_cliente	= Integer(istr_mant.argumento[1])
//ls_embala	= dw_2.Object.emba_codigo[1]
//
//IF dw_2.Object.paen_tipopa[1]=2 THEN RETURN TRUE
//
//SELECT	tpem_cancaj,tpem_altura
//	INTO	:li_cancaj,:altura
//	FROM	dba.tipopallemba
//	WHERE	clie_codigo	=	:li_Cliente
//	AND	emba_codigo	=	:ls_embala
//	AND	tpem_codigo	=	:as_codigo ;
//		
//IF sqlca.SQLCode = -1 THEN
//	F_errorbasedatos(sqlca,"Lectura tabla TipoPallemba")
//	RETURN False
//ELSEIF sqlca.SQLCode = 100 THEN
//	MessageBox("Atención","Tipo de Embalaje no Existe para este Cliente.~rIngrese Otro.", Exclamation!, Ok!)
//	RETURN False
//ELSE
//	istr_mant.Argumento[11]	= String(li_cancaj)
//	dw_2.SetItem(1,"paen_ccajas",li_cancaj)
//	dw_2.SetItem(1,"paen_altura",altura)
//	Cuentacajas()
//	RETURN True
//END IF
//
RETURN False
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
	dw_2.Object.rfpe_nrores.Protect	=	0
	dw_2.Object.rfpe_nrores.BackGround.Color = RGB(255,255,255)
	dw_2.SetColumn("rfpe_nrores")
	dw_2.SetFocus()
ELSE
	dw_2.Object.rfpe_nrores.Protect	=	1
	dw_2.Object.rfpe_nrores.BackGround.Color = RGB(166,180,210)
END IF
end subroutine

public subroutine habilitaingreso ();Date		ld_fecha
Integer	li_tarjas,li_tardef
Boolean	lb_estado = True

dw_2.AcceptText()

IF IsNull(dw_2.Object.rfpe_nrores[1]) OR dw_2.Object.rfpe_nrores[1] = 0 THEN
	lb_estado = False
END IF

pb_ins_det.Enabled = lb_estado
end subroutine

public function boolean existepallet (string ls_columna);Integer	li_cliente, li_cantid, li_planta, li_Estado
Long		ll_nropal, ll_pcopda
Boolean	lb_retorno = False

li_cliente					=	Integer(dw_2.GetItemNumber(1, "clie_codigo"))
li_planta					=	Integer(dw_2.GetItemNumber(1, "plde_codigo"))
ll_nropal 					=	Long(ls_columna)
istr_mant.argumento[2]	=	String(ll_nropal)

//SELECT	Count(*)
//	INTO	:li_cantid
//	FROM	dba.palletencab
//	WHERE	clie_codigo	= 	:li_cliente
//	AND	paen_numero   = 	:ll_nropal
//	AND	plde_codigo		=	:li_planta;

SELECT	Count(*)
	INTO	:li_cantid
	FROM	dba.spro_palletencab
	WHERE	clie_codigo	= 	:li_cliente
	AND	paen_numero = 	:ll_nropal
	AND	plde_codigo	=	:li_planta;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Spro_PalletEncab")
	lb_retorno = True
ELSEIF li_cantid > 0 THEN
	SELECT	paen_estado
	INTO	:li_estado
	FROM	dba.spro_palletencab
	WHERE	clie_codigo	= 	:li_cliente
	AND	paen_numero = 	:ll_nropal
	AND	plde_codigo	=	:li_planta;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Spro_PalletEncab")
		lb_retorno = True
	ELSEIF li_Estado = 2 THEN  
		MessageBox("Error","Pallet Ya fue Recepcionado", &
					Information!, Ok!)					
		lb_retorno = True		
	ELSEIF li_Estado	=	1 THEN	// PDA O PC
		This.TriggerEvent("ue_recuperadatos")
		istr_mant.argumento[3] = String(dw_2.Object.espe_codigo[1])
		istr_mant.argumento[4] = String(dw_2.Object.vari_codigo[1])
		istr_mant.argumento[5] = dw_2.Object.vari_nombre[1]
		istr_mant.argumento[6] = String(dw_2.Object.plde_codigo[1])
	
		istr_mant.argumento[9] = String(dw_2.Object.etiq_codigo[1])
			istr_mant.argumento[12] = ""
			
		ExistePlanta(dw_2.Object.plde_codigo[1])
	END IF
END IF

RETURN lb_retorno
end function

public subroutine buscaembalaje ();//Str_busqueda		lstr_busq
//
//dw_2.Modify("buscaembalaje.border = 0")
//dw_2.Modify("buscaembalaje.border = 5")
//
//lstr_busq.Argum[1]	=	istr_mant.Argumento[1]
//
//OpenWithParm(w_busc_embalajesprod, lstr_busq)
//
//lstr_busq	= Message.PowerObjectParm
//
//IF lstr_busq.Argum[2] <> "" THEN
//	istr_mant.Argumento[7]	=	lstr_busq.Argum[2]
//	istr_mant.Argumento[8]	=	lstr_busq.Argum[3]
//	
//	dw_2.setItem(1, "emba_codigo", lstr_busq.Argum[2])
//	dw_2.setItem(1, "emba_nombre", lstr_busq.Argum[3])
//	dw_2.SetItem(1, "tiem_codigo", Integer(lstr_busq.Argum[5]))
//	
//	dw_emba.Retrieve(Integer(istr_mant.Argumento[1]), lstr_busq.Argum[2])
//ELSE
//	dw_2.SetColumn("emba_codigo")
//	dw_2.SetFocus()
//END IF
//
//dw_2.Modify("buscaembalaje.border = 0")
//dw_2.Modify("buscaembalaje.border = 6")
end subroutine

public function boolean existeembalaje (string ls_columna);//String	ls_codigo, ls_nombre
//Integer	li_cliente, li_tipoen
//
//li_cliente	= Integer(istr_mant.argumento[1])
//ls_codigo	= ls_columna
//
//SELECT	emb.emba_nombre, env.enva_tipoen
//	INTO 	:ls_nombre, :li_tipoen
//	FROM	dba.embalajesprod as emb, dba.envases as env
//	WHERE	emb.clie_codigo	= :li_cliente
//	AND	emb.emba_codigo	= :ls_codigo
//	AND	env.enva_tipoen	= emb.enva_tipoen
//	AND	env.enva_codigo	= emb.enva_codigo;
//		
//IF sqlca.SQLCode = -1 THEN
//	F_errorbasedatos(sqlca,"Lectura tabla Emabalajes")
//	RETURN False
//ELSEIF sqlca.SQLCode = 100 THEN
//	MessageBox("Atención","Código de Embalaje no Existe para este Cliente. Ingrese Otro.", Exclamation!, Ok!)
//	RETURN False
//ELSE
//	istr_mant.argumento[7]	=	ls_codigo
//	istr_mant.argumento[8]	=	ls_nombre
//	dw_2.SetItem(1, "emba_nombre", ls_nombre)
//	dw_2.SetItem(1, "tiem_codigo", li_tipoen)
//	RETURN True
//END IF
	RETURN False
end function

public function boolean existe_nrasda (string as_valor);Long	registros
  
  SELECT Count(*)  
    INTO :registros
    FROM dba.Palletencab
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

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

//IF Not dw_1.uf_check_required(0) THEN RETURN False

//IF Not dw_2.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
//	IF dw_1.Update(True, False) = 1 THEN
//		IF dw_2.Update(True, False) = 1 THEN		
//			Commit;
//			
//			IF sqlca.SQLCode <> 0 THEN
//				F_ErrorBaseDatos(sqlca, This.Title)
//				
//				RollBack;
//			ELSE
//				lb_Retorno	=	True
//				
//				dw_1.ResetUpdate()
//				dw_2.ResetUpdate()
//			END IF
//		ELSE
//			F_ErrorBaseDatos(sqlca, This.Title)
//			
//			RollBack;
//		END IF
//	ELSE
//		F_ErrorBaseDatos(sqlca, This.Title)
//	END IF
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
		RollBack;
	END IF
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

public subroutine buscaenvase ();Integer			li_tipoen
Str_busqueda	lstr_busq

//dw_1.Modify("buscaenvase.border = 5")
li_tipoen = dw_2.Object.enva_tipoen[il_fila]

IF IsNull(li_tipoen) = False OR li_tipoen > 0 THEN 
	lstr_busq.argum[1] = String(li_tipoen)
ELSE
	lstr_busq.argum[1] = ""
END IF

OpenWithParm(w_busc_envases, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	dw_2.SetColumn("enva_tipoen")
	dw_2.SetFocus()
ELSE
	dw_2.SetItem(il_fila, "enva_tipoen", Integer(lstr_busq.argum[1]))
	dw_2.SetItem(il_fila, "enva_codigo", Integer(lstr_busq.argum[2]))
	//dw_1.SetItem(il_fila, "envases_enva_pesone", Dec(lstr_busq.argum[4]))
	//dw_1.SetItem(il_fila, "envases_enva_pesobr", Dec(lstr_busq.argum[5]))
//	
//	dw_1.SetColumn("emba_nombre")
//	dw_1.SetFocus()
END IF

//dw_1.Modify("buscaenvase.border = 6")

RETURN
end subroutine

public subroutine buscaenvasecod (string as_embalaje);String ls_nulo
Integer	li_cliente

SetNull(ii_enva_tipoen)
SetNull(ii_enva_codigo)

li_cliente	=	dw_2.Object.clie_codigo[1]

SELECT enva_tipoen,enva_codigo
	INTO :ii_enva_tipoen,:ii_enva_codigo
	FROM	dba.embalajesprod
	WHERE emba_codigo	=	:as_embalaje
	AND	clie_codigo	=	:li_cliente;


IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Embalajesprod")
END IF

	


end subroutine

public function long buscanuevofolio (integer ai_cliente, integer ai_planta);Integer	li_planta, li_tipoins, li_movto
Long		ll_numero,ll_numero2, ll_fin, ll_actual

li_planta	=	ai_Planta

li_movto = 1

SELECT Max(rfpe_numero)
  INTO :ll_numero
  FROM DBA.RECFRUPROCEE
 WHERE plde_codigo = :li_planta;
 
Select como_inicia, como_actual, como_termin
Into	:ll_numero2, :ll_actual, :ll_fin
from DBA.CORRELMOVIMIENTOS 
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
	AND	rfpe_numero	=	:ll_nfolio ;
				
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
String ls_Nulo

SetNull(ls_Nulo)

ii_Packing	=	Integer(ls_Nulo)

x				= 0
y				= 0
This.Height	= 2020
im_menu		= m_principal

IF Not f_validafechatempo(today()) THEN
   Messagebox('Atención','Aclare Fecha Actual Temporada con Informática')
END IF


This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

IF gi_CodExport = 300 THEN dw_2.Object.plde_codigo.dddw.Name	=	"dw_mues_plantadesp"

dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("rfpe_ptaori", dw_ptaori)
//dw_2.GetChild("puer_codigo", dw_puerto)

dw_planta.SetTransObject(sqlca)
dw_ptaori.SetTransObject(sqlca)
//dw_puerto.SetTransObject(sqlca)

dw_planta.Retrieve(1)
//dw_ptaori.Retrieve(gstr_PCamara.CodPlanta)
dw_ptaori.Retrieve(gi_CodPlanta)
//
//dw_planta.Retrieve(1)
//dw_ptaori.Retrieve(gi_codplanta)
//dw_ptaori.Setfilter("plde_tipopl=2")
//dw_ptaori.Filter()
////dw_puerto.Retrieve(900)
////
//dw_ptaori.SetSort("plde_nombre")
//dw_ptaori.Sort( )

//dw_2.GetChild("frre_codigo", dw_fruta)
//dw_fruta.SetTransObject(sqlca)
//dw_fruta.Retrieve()
dw_2.Object.frre_codigo[1] = 1


dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				=	dw_1
istr_mant.solo_consulta	=	False
istr_mant.argumento[14]	=	''
istr_mant.argumento[16]	=	''


pb_nuevo.PostEvent(Clicked!)

SELECT empr_packing 
	INTO :ii_Packing
	FROM DBA.Parempresa;
		
dw_ptaori.Setfilter("plde_codigo="+String(ii_Packing))
dw_ptaori.Filter()
	
dw_2.Object.tica_codigo.Protect	=	1
dw_2.Object.tran_codigo.Protect	=	1
dw_2.Object.rfpe_patent.Protect	=	1
dw_2.Object.rfpe_chofer.Protect	=	1
dw_2.Object.rfpe_ptaori.Protect	=	1
dw_2.Object.rfpe_tipoen.Protect	=	1
		
dw_2.Object.tica_codigo.BackGround.Color		=	RGB(166,180,210)
dw_2.Object.tran_codigo.BackGround.Color		=	RGB(166,180,210)
dw_2.Object.rfpe_patent.BackGround.Color		=	RGB(166,180,210)
dw_2.Object.rfpe_chofer.BackGround.Color		=	RGB(166,180,210)
dw_2.Object.rfpe_ptaori.BackGround.Color		=	RGB(166,180,210)
dw_2.Object.rfpe_tipoen.BackGround.Color		=	RGB(166,180,210)


dw_2.Object.rfpe_tipoen[1] = 1

////dw_2.GetChild("tpem_codigo", dw_emba)

////dw_emba.SetTransObject(sqlca)
////dw_emba.Retrieve(gi_codexport, gs_CodEmbalaje)

istr_mant.argumento[1]	=	String(gi_codplanta)
istr_mant.argumento[3]	=	String(gi_codexport)
istr_mant.argumento[5]	=	'dw_mues_recfruproced'
istr_mant.argumento[6]	= 	'1'
istr_mant.argumento[13] =	'2'
istr_mant.argumento[20] =	'1'
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

//istr_mant.argumento[6]	=	String(gi_CodPlanta)
//istr_mant.argumento[7]	=	gs_CodEmbalaje

//ids_Palletfruta				=	Create DataStore
//ids_Palletfruta.DataObject	=	"dw_mues_spro_palletfruta"
//ids_Palletfruta.SetTransObject(sqlca)

//istr_mant.argumento[3]	=	''			//	especie
//istr_mant.argumento[4]	=	''			//	variedad
//istr_mant.argumento[7]	=	''			//	embalaje
//istr_mant.argumento[35]	=	''			//	productor
//istr_mant.argumento[9]	=	''			//	etiqueta
//istr_mant.argumento[36]	=	''			//	calibre
////istr_mant.argumento[10]	=	''			//	cond_codigo
//istr_mant.argumento[37]	=	''			//	Numero cajas
//
buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"

end event

event ue_borra_detalle;//Long 		ll_borra
//Integer 	li_null
//String 	ls_filtro
//
//Setnull(li_null)
//
//IF dw_1.rowcount()	<	1 THEN
//	dw_1.SetFocus()
//	RETURN
//END IF
//
//SetPointer(HourGlass!)
//
//ib_borrar 	= 	True
//w_main.SetMicroHelp("Validando la eliminación de detalle...")
//
//Message.DoubleParm 	=	 0
//
////This.TriggerEvent ("ue_validaborrar_detalle")
//
//IF Message.DoubleParm 	=	 -1 THEN RETURN
//
//istr_mant.argumento[37]	= 	String( dw_1.GetItemNumber(dw_1.GetRow(),'capr_numero') )
//
//istr_mant.borra	= 	True
//istr_mant.agrega	= 	False
//
//OpenWithParm(iw_mantencion, istr_mant)
//
//istr_mant 	=	 Message.PowerObjectParm
//
//IF istr_mant.respuesta 	= 	1 THEN	
//	IF dw_1.Getrow() > 0 THEN   		// dw_1.DeleteRow(0) = 1
//		//FOR ll_fila	=	1	TO dw_1.RowCount()	
//			dw_1.SetItem(dw_1.GetRow(),'capr_numpal',li_null)
//			dw_1.SetItem(dw_1.GetRow(),'capr_estado',0)
//			dw_1.SetItemStatus(dw_1.GetRow(),0,Primary!,DataModified!)
//		//NEXT
//		ls_filtro	=	"capr_numpal = "+istr_mant.argumento[2]
//		dw_1.SetFilter(ls_filtro)
//		dw_1.Filter()				
//		
//		ib_borrar 	=	 False
//		w_main.SetMicroHelp("Borrando Caja...")
//		ll_borra		=	1
//		SetPointer(Arrow!)
//	ELSE
//		ib_borrar 	=	 False
//		MessageBox(This.Title,"No se puede borrar actual registro.")
//	END IF
//
//	IF ll_borra	=	1 THEN    // dw_1.RowCount() = 0 
//		HabilitaEncab(True)
//		pb_eli_det.Enabled 	=	 False
//	END IF
//END IF
//
//istr_mant.borra	 = 	False
end event

event ue_nuevo_detalle;Integer	li_Cajas

istr_mant.Borra		=	False
istr_mant.Agrega		=	True
dw_2.Enabled			=	False
Message.DoubleParm	=	0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

OpenWithParm(w_maed_palletencab_recepcion_granel, istr_mant)
		
IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
		
IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE	
END IF
		
dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
		

	







//istr_mant.borra	= False
//istr_mant.agrega	= True
////cuentacajas()
//istr_mant.argumento[28]=String(dw_2.object.paen_feccon[1])
//
//OpenWithParm(iw_mantencion, istr_mant)
//
//istr_mant	 = Message.PowerObjectParm
//
////IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
////
////IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
////	pb_eliminar.Enabled	= TRUE
////	pb_grabar.Enabled		= TRUE
////END IF
////dw_1.SetRow(il_fila)
////dw_1.SelectRow(il_fila,True)
//
//IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
//
//IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
//	//pb_eliminar.Enabled	= TRUE
//	pb_grabar.Enabled	= TRUE
//END IF
//
//dw_1.SetRow(il_fila)
//dw_1.SelectRow(il_fila,True)
//
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
				//pb_eliminar.Enabled	= True
				//pb_grabar.Enabled		= True
				pb_imprimir.Enabled	= True
				pb_ins_det.Enabled	= True

				IF ll_fila_d > 0 THEN
					pb_eli_det.Enabled	= True
					
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
					
					istr_mant.Argumento[4]	=	String(dw_2.Object.rfpe_tarjas[1])
					istr_mant.Argumento[11]	=	String(dw_2.Object.rfpe_tardef[1])
					istr_mant.argumento[21] =  String(dw_2.Object.rfpe_ptaori[1])
					istr_mant.argumento[30] =  String(dw_2.Object.rfpe_nrores[1])
		
					istr_mant.argumento[40] = ''
					IF dw_2.Object.rfpe_tipoen[1] = 2 OR dw_2.Object.rfpe_tipoen[1] =	6 THEN
						istr_mant.argumento[40] = '1'
					END IF											
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

on w_maed_recfruprocee_granel.create
call super::create
end on

on w_maed_recfruprocee_granel.destroy
call super::destroy
end on

event ue_nuevo;Integer li_codigo

HabilitaEncab(True)
istr_mant.argumento[35] =	'0'
istr_mant.argumento[36] =	'0'
ib_ok	= True

CHOOSE CASE wf_modifica()
	CASE -1
		ib_ok = False
	CASE 1
		Message.DoubleParm = 0
		IF ii_borra = 0 THEN 
		   This.TriggerEvent("ue_guardar")
		ELSE
			ii_borra = 0
		END IF
		IF message.DoubleParm = -1 THEN ib_ok = False
	CASE 3
		ib_ok	= False
		RETURN
END CHOOSE

IF ib_ok = False THEN RETURN

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
//dw_2.SetItem(1, "tica_codigo", 1)
//dw_2.SetItem(1, "frre_codigo", 1)
dw_2.SetItem(1, "rfpe_horrec",Now())
dw_2.Object.rfpe_ptaori[1]	=	ii_Packing
	
//dw_ptaori.SetSort("plde_nombre")
//dw_ptaori.Sort( )

/*se reasigna tipo de recpción como Packing*/
istr_mant.argumento[31]='1'
/*se limpia argumento de recepción transmitida*/
istr_mant.argumento[33] = ''
//

dw_2.Object.clie_codigo.Protect	=	1
dw_2.Object.plde_codigo.Protect	=	1
dw_2.Object.rfpe_tarjas.Protect	=	1
dw_2.Object.rfpe_tardef.Protect	=	1
dw_2.Object.rfpe_fecrec.Protect	=	1
dw_2.Object.rfpe_horrec.Protect	=	1
dw_2.Object.rfpe_nomres.Protect	=	1
dw_2.Object.frre_codigo.Protect	=	1
dw_2.Object.clie_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.plde_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.rfpe_tarjas.BackGround.Color = RGB(166,180,210)
dw_2.Object.rfpe_tardef.BackGround.Color = RGB(166,180,210)
dw_2.Object.rfpe_fecrec.BackGround.Color = RGB(166,180,210)
dw_2.Object.rfpe_horrec.BackGround.Color = RGB(166,180,210)
dw_2.Object.rfpe_nomres.BackGround.Color = RGB(166,180,210)
dw_2.Object.frre_codigo.BackGround.Color = RGB(166,180,210)

dw_2.SetRedraw(True)

dw_2.SetFocus()
HabilitaEncab(True)






//
//dw_2.SetItem(1, "clie_codigo", Integer(istr_mant.Argumento[1]))
//dw_2.SetItem(1, "plde_codigo", Integer(istr_mant.Argumento[6]))
////dw_2.SetItem(1, "espe_codigo", gi_CodEspecie)
//
////istr_mant.argumento[3]	=	String(gi_CodEspecie)
////istr_mant.argumento[9]	=	"1"
//istr_mant.argumento[10]	=	"0"
//
//istr_mant.argumento[3]	=	''			//	especie
//istr_mant.argumento[4]	=	''			//	variedad
//istr_mant.argumento[7]	=	''			//	embalaje
//istr_mant.argumento[35]	=	''			//	productor
//istr_mant.argumento[9]	=	''			//	etiqueta
//istr_mant.argumento[36]	=	''			//	calibre
////istr_mant.argumento[10]	=	''			//	cond_codigo
//istr_mant.argumento[37]	=	''			//	Numero cajas
//
//
//dw_2.Object.vari_codigo.Protect	=	0
//dw_2.Object.vari_codigo.BackGround.Color = RGB(255,255,255)
//
//dw_3.Reset()
end event

event ue_seleccion;call super::ue_seleccion;Integer	li_cliente, li_planta
Long		ll_nropal, ll_pcopda

istr_busq.argum[1]	=	String(dw_2.Object.clie_codigo[1]) 
istr_busq.argum[2]	=	""
istr_busq.argum[5]	=  ""

OpenWithParm(w_busc_palletencab, istr_busq)

istr_busq = Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	istr_mant.argumento[1] = istr_busq.argum[1]
	istr_mant.argumento[2] = istr_busq.argum[2]
	istr_mant.argumento[6] = istr_busq.argum[7]
	
	li_cliente					=	Integer(istr_mant.argumento[1])
	li_planta					=	Integer(istr_mant.argumento[7])
	ll_nropal 					=	Integer(istr_mant.argumento[2])

	
	SELECT	paen_pcopda
		INTO	:ll_pcopda
		FROM	dba.palletencab
		WHERE	clie_codigo		= 	:li_cliente
		AND		paen_numero   = 	:ll_nropal
		AND		plde_codigo		=	:li_planta;
		
	IF ll_pcopda =	2	THEN
		This.TriggerEvent("ue_recuperadatos")
		ExistePallet(string(dw_2.Object.paen_numero[1]))
	ELSE
		MessageBox("Atención","El Numero de Pallet Asociado No es un Ingreso Caja a Caja. Ingrese Otro.", Exclamation!, Ok!)
		pb_buscar.SetFocus()
	END IF
ELSE
	pb_buscar.SetFocus()
END IF
end event

event ue_antesguardar;//Long 		ll_nuevofolio
//Integer	li_fillas, ll_filas, ll_fila_d, ll_fila_g
//Date ld_Fecha_Ing
//
//IF dw_2.GetNextModified(0, Primary!) > 0 THEN
//	dw_2.SetItem(1, "rfpe_fecact", Today())
//	dw_2.SetItem(1, "rfpe_horact", Now())
//END IF
//
//ll_nuevofolio = dw_2.Object.rfpe_numero[1]
//
////IF Not ib_primera_entrada AND Not ib_existe_folio  THEN	
//	/*
//	Se actualiza tabla recfruprocee a objeto de bloquearla hasta que termine la grabación
//	del ingreso
//	*/
//	UPDATE dba.RECFRUPROCEE SET
//			 rfpe_guides = 999
//			 WHERE rfpe_tarjas = 999
//			 AND   rfpe_nrores = 999
//			 AND   rfpe_tardef = 999;
//			 
//	IF isnull(dw_2.Object.rfpe_numero[1]) OR dw_2.Object.rfpe_numero[1] = 0 THEN
//		ll_nuevofolio=Buscanuevofolio(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]))
//	END IF
//	
//	IF Long(ll_nuevofolio) = 0  THEN
//		MessageBox("Advertencia","No Quedan Correlativos Disponibles, Proceda por Mantención.")
//		Message.DoubleParm = -1
//		Return 
//	END IF
//	
//	IF isnull(dw_2.Object.rfpe_numero[1]) OR dw_2.Object.rfpe_numero[1] = 0 THEN
//		dw_2.Object.rfpe_numero[1]	= ll_nuevofolio
//   	dw_2.SetItem(1, "rfpe_numero",ll_nuevofolio)
//	END IF	
//	
//	dw_2.Object.rfpe_tipoen[1] = 	1
//
//	istr_mant.argumento[2]	= String(ll_nuevofolio)
//	
//	FOR li_fillas = 1 TO dw_1.RowCount()
//		 dw_1.Object.rfpe_numero[li_fillas]	= ll_nuevofolio
//	NEXT		
//
Long			ll_nrodoc
Integer		li_Cliente, li_Planta

IF	ib_Borrar	=	False	THEN
	li_Cliente	=	dw_2.Object.clie_codigo[1]
	li_Planta	=	dw_2.Object.plde_codigo[1]
	
	dw_2.Object.rfpe_pcopda[1]	=	2
	
	IF dw_2.GetNextModified(0, Primary!) > 0 THEN
		dw_2.Object.rfpe_fecact[1]	=	Today()
		dw_2.Object.rfpe_horact[1]	=	Now()		
	END IF
	
	//	Determina Correlativo 
	IF dw_2.GetItemStatus(1, 0, Primary!) = New! OR &
		dw_2.GetItemStatus(1, 0, Primary!) = NewModified! THEN			
		//	Obtención de Folio 
		IF IsNull(istr_mant.Argumento[2]) OR  Long(istr_mant.Argumento[2]) = 0 THEN
			
			// Bloquea Grabación
			Integer li_expo
			SELECT expo_codigo INTO :li_expo
			FROM dba.parempresa;
			UPDATE dba.parempresa SET
			expo_codigo = li_expo;
	
			//ll_nrodoc	=	Buscanuevofolio(Integer(istr_mant.argumento[1]))		
			ll_nrodoc=Buscanuevofolio(Integer(istr_mant.argumento[3]),Integer(istr_mant.argumento[1]))
			
			IF Long(ll_nrodoc) = 0  THEN
				MessageBox("Advertencia","No Quedan Correlativos Disponibles, Proceda por Mantención.")
				Message.DoubleParm = -1
				Return 
			END IF
			
			istr_mant.Argumento[2]		=	String(ll_nrodoc)
			dw_2.Object.rfpe_numero[1]	=	ll_nrodoc
			
			dw_2.Object.rfpe_tipoen[1] = 	1
			
		END IF
	
	END IF
END IF	

end event

event ue_modifica_detalle;//Cuentacajas()
//
//IF dw_1.RowCount() > 0 THEN
//	istr_mant.agrega = False
//
//
////	istr_mant.argumento[4]	=	String(dw_1.Object.vari_codigo[il_fila]) 
////	istr_mant.argumento[7]	=	dw_1.Object.emba_codigo[il_fila] 
//	
//	OpenWithParm(iw_mantencion, istr_mant)
//END IF
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

event ue_guardar;Boolean lb_borrado

IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db(False) THEN
	w_main.SetMicroHelp("Información Grabada.")
	//pb_eliminar.Enabled	= True
	pb_imprimir.Enabled	= True
	//TriggerEvent("ue_nuevo")
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	//pb_Eliminar.Enabled	=	True
	
	IF istr_mant.Solo_Consulta THEN
		dw_2.Enabled			=	False
//		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled		=	False
		pb_ins_det.Enabled	=	False
		pb_eli_det.Enabled	=	False
	ELSE
		dw_2.Enabled			=	True
		//pb_Eliminar.Enabled	=	True
		pb_Grabar.Enabled		=	True
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

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
//str_info	lstr_info

istr_info.titulo	= "RECEPCION DE PALLETS"
istr_info.copias	= 1

OpenWithParm(vinf,istr_info)

vinf.dw_1.DataObject = "dw_info_generapalletdesdecajasgranel" //"dw_info_generapalletdesdecajas" 

vinf.dw_1.GetChild("plde_codigo", dw_planta)
vinf.dw_1.GetChild("espe_codigo", dw_especie)
vinf.dw_1.GetChild("etiq_codigo", dw_etiqueta)
//vinf.dw_1.GetChild("cond_codigo", dw_condiciones)

vinf.dw_1.SetTransObject(sqlca)
dw_planta.SetTransObject(sqlca)
dw_etiqueta.SetTransObject(sqlca)
dw_especie.SetTransObject(sqlca)
dw_condiciones.SetTransObject(sqlca)

dw_planta.Retrieve()

dw_especie.Retrieve()

dw_etiqueta.Retrieve()

dw_condiciones.Retrieve(integer(istr_mant.Argumento[10]))

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[6]),Long(istr_mant.argumento[2]))		

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

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_recfruprocee_granel
integer x = 142
integer y = 1092
integer width = 2962
integer height = 952
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_recfruproced_granel"
end type

event dw_1::doubleclicked;//

end event

event dw_1::clicked;//
end event

event dw_1::sqlpreview;DwItemStatus	Estado

IF Row = 0 THEN RETURN 0

CHOOSE CASE Buffer
	CASE Delete!
		This.TriggerEvent("on_delete")
		
	CASE Primary!, Filter!
		Estado	=	This.GetItemStatus(Row, 0, Buffer)
		
		CHOOSE CASE Estado
			CASE New!, NewModified!
				This.TriggerEvent("on_insert")
				
			CASE DataModified!
				This.TriggerEvent("on_update")
				
		END CHOOSE
END CHOOSE
	
IF Message.ReturnValue = 1 THEN
	RETURN 1
ELSE
	RETURN 0
END IF
end event

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_recfruprocee_granel
integer x = 73
integer y = 56
integer width = 3154
integer height = 996
string dataobject = "dw_mant_recfruprocee_granel"
end type

event dw_2::itemchanged;call super::itemchanged;//Long		ll_null, ll_PalletMan, ll_Data
//String	ls_columna, ls_asda
//Date		ld_nula
//Integer	li_FilaMan
//
//DataWIndowChild	dw_calibres
//
//SetNull(ll_null)
//SetNull(ld_nula)
//
//ls_columna = GetColumnName()
//
//ii_yaexiste	=	0
//
//CHOOSE CASE ls_columna
//	CASE "clie_codigo"
//		IF F_ValidaCliente(Integer(data)) THEN
//			istr_mant.argumento[1]	= data
//			dw_especie.Retrieve()
//			dw_etiqueta.Retrieve()
//			
//			dw_2.GetChild("plde_codigo", dw_planta)
//			dw_planta.SetTransObject(sqlca)
//			dw_planta.Retrieve(1)
//			
////			IF EmbalajeCliente(Integer(data)) = "" THEN
////				MessageBox("Atención", "Cliente no tiene definido Tipos de Pallets" + &
////								" por Embalaje.~r~rIngrese o seleccione otro Cliente.")
////				dw_2.SetItem(1, "clie_codigo", gi_codexport)
////				RETURN 1
////			END IF	
//		ELSE
//			dw_2.SetItem(1, "clie_codigo", gi_codexport)
//			RETURN 1
//		END IF 
//	CASE "paen_numero"
//		ll_PalletMan = dw_2.Object.paen_numero[1]
//		
//		li_FilaMan	 = Integer(GetRow())
//		
//		IF IsNull(ll_PalletMan) OR ll_PalletMan=0 THEN
//			ll_Data = Long(Data)
//		ELSE
//			ll_Data = ll_PalletMan
//		END IF
//		
//		IF ExistePallet(String(ll_Data)) OR IsNull(ll_Data) THEN
//			ii_yaexiste = 1
//			dw_2.SetItem(1, "paen_numero", ll_null)
//			RETURN 1
//		END IF
//		HabilitaIngreso()
//	
//	CASE "espe_codigo"
//		istr_mant.argumento[3]	= data
//		dw_2.SetItem(1, "vari_nombre", "")
//		dw_2.SetItem(1, "vari_codigo", ll_null)
//				
//	CASE "vari_codigo"
//		IF ExisteVariedad(data) = False THEN
//			dw_2.SetItem(1, "vari_nombre", "")
//			dw_2.SetItem(1, "vari_codigo", ll_null)
//			RETURN 1
//		END IF
//					
//	CASE "plde_codigo"
//		istr_mant.argumento[6]	= data
//		
////	CASE "emba_codigo"
////		IF ExisteEmbalaje(data) = False THEN
////			dw_2.SetItem(1, "emba_nombre", "")
////			dw_2.SetItem(1, "emba_codigo", ll_null)
////			RETURN 1
////		ELSE
////		 dw_emba.Retrieve(Integer(istr_mant.argumento[1]),istr_mant.argumento[7])
////		END IF
//			
//	CASE "tpem_codigo"
//		IF dw_2.object.paen_tipopa[Row] = 1 THEN
////			IF ExisteTipoEmbalaje(data) = False THEN
////				dw_2.SetItem(1, "emba_codigo", ll_null)
////				RETURN 1
////			END IF
//		END IF
//
//	CASE "etiq_codigo"
//		istr_mant.argumento[9]	= data
//		
////	CASE "cond_codigo"
////		istr_mant.argumento[10]	= data
//			
//	CASE "paen_ccajas"
//		istr_mant.argumento[11]	= data
//			
//	CASE "paen_pexpor"
//		istr_mant.argumento[22]	= data
//			
//	CASE "paen_pmixto"
//		istr_mant.argumento[23]	= data
//
//
//	CASE "paen_fecemb"
//		IF Not f_validafechatempo(date(data)) THEN
//			This.SetItem(Row, "paen_fecemb", ld_nula)
//			RETURN 1
//		ELSE
//			istr_mant.Argumento[40] = Data
//		END IF
//		
//	CASE "paen_cosecha"
//		IF Not f_validafechatempo(date(data)) THEN
//			This.SetItem(Row, "paen_cosecha", ld_nula)
//			RETURN 1
//		END IF
//		
//	CASE "paen_nrasda"
//		IF data<>'' OR NOT IsNull(data)  THEN
//			istr_mant.argumento[32] = 'RIOBL'+ data
//			data = istr_mant.argumento[32]
//		END IF
//		
//		IF Existe_nrasda(data) OR IsNull(data) THEN
//			dw_2.SetItem(1, "paen_nrasda", String(ll_null))
//			RETURN 1
//		END IF
//		
//	
//		
//	
////	CASE "dest_codigo"
////		istr_mant.argumento[33]	= data
////		IF Isnull(data) or data = " " THEN
////			pb_ins_det.Enabled	=	False
////			dw_2.SetItem(1, "dest_codigo", ll_null)
////			RETURN 1
////		END IF
//
//END CHOOSE
//



String	ls_columna, ls_nula
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
		
	CASE "rfpe_tarjas"
		istr_mant.argumento[4]	= data
		
	CASE "rfpe_tardef"
		istr_mant.argumento[11]	= data

	CASE "clie_codigo"
		IF NoExisteCliente(Integer(data)) THEN
			This.SetItem(Row, ls_Columna, gi_codexport)
			RETURN 1
		END IF	
		
	CASE "rfpe_fecrec"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, ls_Columna, ld_nula)
			RETURN 1
		END IF

	CASE "rfpe_nrores"
		istr_mant.argumento[30]	= data

END CHOOSE

HabilitaIngreso()
end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_recfruprocee_granel
integer y = 252
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_recfruprocee_granel
boolean visible = false
integer y = 480
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_recfruprocee_granel
integer y = 688
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_recfruprocee_granel
integer y = 896
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_recfruprocee_granel
integer y = 1156
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_recfruprocee_granel
integer x = 3296
integer y = 1484
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_recfruprocee_granel
boolean visible = false
integer x = 3301
integer y = 1660
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_recfruprocee_granel
boolean visible = false
integer y = 76
end type

