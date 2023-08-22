$PBExportHeader$w_maed_generapalletdesdecajas_granel.srw
forward
global type w_maed_generapalletdesdecajas_granel from w_mant_encab_deta_csd
end type
type dw_3 from datawindow within w_maed_generapalletdesdecajas_granel
end type
end forward

shared variables

end variables

global type w_maed_generapalletdesdecajas_granel from w_mant_encab_deta_csd
integer width = 3547
integer height = 2200
string title = "GENERA PALLETIZADO GRANEL"
string menuname = ""
boolean controlmenu = false
boolean minbox = false
dw_3 dw_3
end type
global w_maed_generapalletdesdecajas_granel w_maed_generapalletdesdecajas_granel

type variables
Integer	ii_yaexiste, ii_enva_tipoen, ii_enva_codigo

w_mant_deta_generapalletdesdecajas_grane  iw_mantencion

DataWindowChild	dw_especie, dw_etiqueta, dw_planta,&
                  dw_cliente,dw_condiciones,dw_emba
						
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
protected function boolean wf_actualiza_db_cajas (boolean borrando)
public subroutine buscaenvase ()
public subroutine buscaenvasecod (string as_embalaje)
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
	dw_2.Object.paen_palneo.Protect	=	0
	dw_2.Object.paen_numero.Protect	=	0
	dw_2.Object.copa_codigo.Protect	=	0
	dw_2.Object.paen_palneo.BackGround.Color = RGB(255,255,255)
	dw_2.Object.paen_numero.BackGround.Color = RGB(255,255,255)
	dw_2.Object.copa_codigo.BackGround.Color = RGB(255,255,255)
	dw_2.SetColumn("paen_palneo")
	dw_2.SetFocus()
ELSE
	dw_2.Object.paen_palneo.Protect	=	1
	dw_2.Object.paen_numero.Protect	=	1
	dw_2.Object.copa_codigo.Protect	=	1
	dw_2.Object.paen_palneo.BackGround.Color = RGB(166,180,210)
	dw_2.Object.paen_numero.BackGround.Color = RGB(166,180,210)
	dw_2.Object.copa_codigo.BackGround.Color = RGB(166,180,210)
END IF
end subroutine

public subroutine habilitaingreso ();Date		ld_fecha
Boolean	lb_estado = True

dw_2.AcceptText()

//IF IsNull(dw_2.Object.paen_numero[1]) OR dw_2.Object.paen_numero[1] = 0 OR &
//	IsNull(dw_2.Object.espe_codigo[1]) OR dw_2.Object.espe_codigo[1] = 0 OR &
//	IsNull(dw_2.Object.vari_codigo[1]) OR dw_2.Object.vari_codigo[1] = 0 OR &
//	IsNull(dw_2.Object.emba_codigo[1]) OR dw_2.Object.emba_codigo[1] = "" OR &
//	IsNull(dw_2.Object.etiq_codigo[1]) OR &
//	IsNull(dw_2.Object.cate_codigo[1]) OR dw_2.Object.cate_codigo[1] = 0 OR &
//	IsNull(dw_2.Object.paen_fecemb[1]) OR dw_2.Object.paen_fecemb[1] = ld_fecha OR &
//	IsNull(dw_2.Object.paen_cosecha[1])OR dw_2.Object.paen_cosecha[1] = ld_fecha OR &
//	IsNull(dw_2.Object.stat_codigo[1]) OR dw_2.Object.stat_codigo[1] = 0 OR &
//	IsNull(dw_2.Object.paen_ccajas[1]) OR dw_2.Object.paen_ccajas[1] = 0 OR &	
//	IsNull(dw_2.Object.frio_codigo[1]) OR dw_2.Object.frio_codigo[1] = ""  THEN


IF IsNull(dw_2.Object.clie_codigo[1]) OR dw_2.Object.clie_codigo[1] = 0 OR &
	IsNull(dw_2.Object.plde_codigo[1]) OR dw_2.Object.plde_codigo[1] = 0 OR &
	IsNull(dw_2.Object.paen_numero[1]) OR dw_2.Object.paen_numero[1] = 0 OR &
	IsNull(dw_2.Object.copa_codigo[1])	THEN
		
	lb_Estado = False
END IF
//IF IsNull(dw_2.Object.paen_numero[1]) THEN
//	lb_estado = False
//ELSEIF dw_2.Object.paen_tipopa[1] = 1 AND &
//	IsNull(dw_2.Object.tpem_codigo[1]) OR dw_2.Object.tpem_codigo[1] = '' THEN
//	lb_estado = False
//END IF

//IF	IsNull(dw_2.Object.dest_codigo[1]) OR dw_2.Object.dest_codigo[1] = 0 THEN
//	lb_estado = False
//END IF

/* Valida que fecha ingresada este dentro del rango*/
//IF lb_estado = TRUE THEN
//	IF NOT controla_fecha(dw_2.Object.paen_fecemb[1]) THEN
//		lb_estado = False
//	ELSEIF NOT controla_fecha(dw_2.Object.paen_cosecha[1]) THEN
//		lb_estado = False
//	END IF
//END IF

pb_ins_det.Enabled	= 	lb_estado
pb_grabar.Enabled 	=	lb_estado
pb_eli_det.Enabled	=	lb_estado
end subroutine

public function boolean existepallet (string ls_columna);Integer	li_cliente,li_Existe,li_Existe2,li_planta,li_Estado
Long		ll_nropal,ll_pcopda
Boolean	lb_Retorno

lb_Retorno = False

li_cliente					=	Integer(dw_2.GetItemNumber(1, "clie_codigo"))
li_planta					=	Integer(dw_2.GetItemNumber(1, "plde_codigo"))
ll_nropal 					=	Long(ls_columna)

SELECT	Count(*)
	INTO	:li_Existe
	FROM	dba.spro_palletencab
	WHERE	clie_codigo	= 	:li_cliente
	AND	paen_numero = 	:ll_nropal
	AND	plde_codigo	=	:li_planta;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Spro_PalletEncab")
	lb_retorno = True
ELSEIF IsNull(li_Existe) OR li_Existe = 0 THEN	
			SELECT Count(*)
				INTO  :li_Existe2
				FROM  dba.palletencab
				WHERE clie_codigo = :li_cliente
				AND   plde_codigo = :li_planta
				AND   paen_numero = :ll_nropal;
	
			IF sqlca.SQLCode = -1 THEN
				F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Palletencab")
				lb_Retorno = True
			ELSEIF sqlca.SQLCode <> 0  OR li_Existe2 > 0 THEN
					MessageBox("Error","Pallet Existe en Frigorífico", &
					Information!, Ok!)
					lb_Retorno = True
			ELSE
			      lb_Retorno = False
			END IF
ELSEIF li_Existe > 0 THEN
	SELECT	paen_estado
	INTO	:li_estado
	FROM	dba.spro_palletencab
	WHERE	clie_codigo	= 	:li_cliente
	AND	paen_numero = 	:ll_nropal
	AND	plde_codigo	=	:li_planta;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Spro_PalletEncab")
		lb_Retorno = True
//	ELSEIF li_Estado = 2 THEN  
//		MessageBox("Error","Pallet Ya fue Recepcionado", &
//					Information!, Ok!)					
//		lb_Retorno = True		
	ELSEIF li_Estado	=	1 THEN	// PDA O PC
		This.TriggerEvent("ue_recuperadatos")
		istr_mant.argumento[3] = String(dw_2.Object.espe_codigo[1])
		istr_mant.argumento[4] = String(dw_2.Object.vari_codigo[1])
		istr_mant.argumento[5] = dw_2.Object.vari_nombre[1]
		istr_mant.argumento[6] = String(dw_2.Object.plde_codigo[1])
	
		istr_mant.argumento[9] = String(dw_2.Object.etiq_codigo[1])
			istr_mant.argumento[12] = ""
			
		ExistePlanta(dw_2.Object.plde_codigo[1])
	ELSE
			MessageBox("Error","Pallet Ya No Está en Existencia Packing.")
			lb_Retorno = True
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

//IF Not dw_2.uf_check_required(0) THEN RETURN False

//IF Not dw_3.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
	//IF dw_3.Update(True, False) = 1 THEN
		//IF dw_2.Update(True, False) = 1 THEN		
		IF ids_Palletfruta.Update(True, False) = 1 THEN   	//  Store		
			Commit;
			
			IF sqlca.SQLCode <> 0 THEN
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			ELSE
				lb_Retorno	=	True
				
				ids_Palletfruta.ResetUpdate()
			END IF
		ELSE
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		END IF
	//ELSE
	//	F_ErrorBaseDatos(sqlca, This.Title)
	//END IF
ELSE
	//IF wf_actualiza_db_cajas(false) THEN						//  palletencab  dw_2.Update(True, False) 
	IF dw_1.Update(True, False) = 1 THEN  	
		IF dw_2.Update(True, False) = 1 THEN   			//  palletfruta   dw_3
			IF dw_3.Update(True, False) = 1  THEN   		//  spro_cajasprod  wf_actualiza_db_cajas(false)
				Commit;
					
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
						
					RollBack;
				ELSE
					lb_Retorno	=	True
						
					dw_3.ResetUpdate()
					dw_2.ResetUpdate()
					dw_1.ResetUpdate()
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
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

protected function boolean wf_actualiza_db_cajas (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno
long ll_fila

DwItemStatus	Estadol
//IF Not dw_2.uf_check_required(0) THEN RETURN False

//IF Not dw_1.uf_validate(0) THEN RETURN False

//FOR ll_fila = 1 TO dw_1.RowCount()		 
//	Estadol	=	dw_1.GetItemStatus(ll_fila, 0, Primary!)
//NEXT

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF Borrando THEN
//	IF dw_3.Update(True, False) = 1 THEN
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
//				dw_3.ResetUpdate()
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

	IF dw_1.Update(True, False) = 1 THEN   			//  spro_cajasprod
		Commit;
					
		IF sqlca.SQLCode <> 0 THEN
			F_ErrorBaseDatos(sqlca, This.Title)
						
			RollBack;
		ELSE
			lb_Retorno	=	True
			
			dw_1.ResetUpdate()
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

dw_2.GetChild("clie_codigo", dw_cliente)
dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("espe_codigo", dw_especie)
dw_2.GetChild("etiq_codigo", dw_etiqueta)
//dw_2.GetChild("tpem_codigo", dw_emba)

dw_cliente.SetTransObject(sqlca)
dw_planta.SetTransObject(sqlca)
dw_especie.SetTransObject(sqlca)
dw_etiqueta.SetTransObject(sqlca)
//dw_emba.SetTransObject(sqlca)

dw_cliente.Retrieve()
dw_planta.Retrieve(1)
dw_especie.Retrieve()
dw_etiqueta.Retrieve()
//dw_emba.Retrieve(gi_codexport, gs_CodEmbalaje)

istr_mant.argumento[1]	=	String(gi_CodExport)
istr_mant.argumento[6]	=	String(gi_CodPlanta)
istr_mant.argumento[7]	=	gs_CodEmbalaje
istr_mant.argumento[22]	=	'1'		//	Pallet Exportación
istr_mant.argumento[23]	=	'0'		//	Pallet Mixto

dw_2.Object.paen_pmixto.Protect	=	1

dw_3.SetTransObject(sqlca)

istr_mant.argumento[3]	=	''			//	especie
istr_mant.argumento[4]	=	''			//	variedad
istr_mant.argumento[7]	=	''			//	embalaje
istr_mant.argumento[35]	=	''			//	productor
istr_mant.argumento[9]	=	''			//	etiqueta
istr_mant.argumento[36]	=	''			//	calibre
//istr_mant.argumento[10]	=	''			//	cond_codigo
istr_mant.argumento[37]	=	''			//	Numero cajas

buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"

end event

event ue_borra_detalle;Long 		ll_borra
Integer 	li_null
String 	ls_filtro

Setnull(li_null)

IF dw_1.rowcount()	<	1 THEN
	dw_1.SetFocus()
	RETURN
END IF

SetPointer(HourGlass!)

ib_borrar 	= 	True
w_main.SetMicroHelp("Validando la eliminación de detalle...")

Message.DoubleParm 	=	 0

//This.TriggerEvent ("ue_validaborrar_detalle")

IF Message.DoubleParm 	=	 -1 THEN RETURN

istr_mant.argumento[37]	= 	String( dw_1.GetItemNumber(dw_1.GetRow(),'capr_numero') )

istr_mant.borra	= 	True
istr_mant.agrega	= 	False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant 	=	 Message.PowerObjectParm

IF istr_mant.respuesta 	= 	1 THEN	
	IF dw_1.Getrow() > 0 THEN   		// dw_1.DeleteRow(0) = 1
		//FOR ll_fila	=	1	TO dw_1.RowCount()	
			dw_1.SetItem(dw_1.GetRow(),'capr_numpal',li_null)
			dw_1.SetItem(dw_1.GetRow(),'capr_estado',0)
			dw_1.SetItemStatus(dw_1.GetRow(),0,Primary!,DataModified!)
		//NEXT
		ls_filtro	=	"capr_numpal = "+istr_mant.argumento[2]
		dw_1.SetFilter(ls_filtro)
		dw_1.Filter()				
		
		ib_borrar 	=	 False
		w_main.SetMicroHelp("Borrando Caja...")
		ll_borra		=	1
		SetPointer(Arrow!)
	ELSE
		ib_borrar 	=	 False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

	IF ll_borra	=	1 THEN    // dw_1.RowCount() = 0 
		HabilitaEncab(True)
		pb_eli_det.Enabled 	=	 False
	END IF
END IF

istr_mant.borra	 = 	False
end event

event ue_nuevo_detalle;istr_mant.borra	= False
istr_mant.agrega	= True
//cuentacajas()
istr_mant.argumento[28]=String(dw_2.object.paen_feccon[1])

OpenWithParm(iw_mantencion, istr_mant)

istr_mant	 = Message.PowerObjectParm

//IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
//
//IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
//	pb_eliminar.Enabled	= TRUE
//	pb_grabar.Enabled		= TRUE
//END IF
//dw_1.SetRow(il_fila)
//dw_1.SelectRow(il_fila,True)

IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	//pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled	= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila_d, ll_fila_e, respuesta, ll_fila_c, ll_fila_s
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), &
									    Integer(istr_mant.argumento[6]), &
				 						 Long(istr_mant.argumento[2]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos 1.", &
										Information!, RetryCancel!)
	ELSE		
		DO
			ll_fila_c	= dw_1.Retrieve(Integer(istr_mant.argumento[6]), &
												 Long(istr_mant.argumento[2]), &	
												 Integer(istr_mant.argumento[1]))
									
			ll_fila_d	= dw_3.Retrieve(Integer(istr_mant.argumento[1]), & 
												 Long(istr_mant.argumento[2]), &
												 Integer(istr_mant.argumento[6]))
		
			IF ll_fila_s = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos 2.", &
												Information!, RetryCancel!)
			ELSE
				//pb_eliminar.Enabled	= True
				pb_grabar.Enabled		= True
				pb_imprimir.Enabled	= True
				pb_ins_det.Enabled	= True
				IF ll_fila_c > 0 THEN
					pb_eli_det.Enabled = True										
					IF Len(Trim(istr_mant.argumento[1])) <= 3 THEN					
						dw_2.Object.paen_palneo[1]	=	Fill('0',3 - Len(Trim(istr_mant.argumento[1])) ) + Trim(istr_mant.argumento[1]) +&
															Trim(istr_mant.argumento[2])
					END IF
					dw_1.SetRow(1)
					dw_1.SelectRow(1,True)
					dw_1.SetFocus()
					HabilitaEncab(False)
				ELSE
					pb_ins_det.SetFocus()
				END IF
			END IF
		LOOP WHILE respuesta = 1

		IF respuesta = 2 THEN Close(This)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_maed_generapalletdesdecajas_granel.create
int iCurrent
call super::create
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
end on

on w_maed_generapalletdesdecajas_granel.destroy
call super::destroy
destroy(this.dw_3)
end on

event ue_nuevo;call super::ue_nuevo;HabilitaEncab(True)

dw_2.SetItem(1, "clie_codigo", Integer(istr_mant.Argumento[1]))
dw_2.SetItem(1, "plde_codigo", Integer(istr_mant.Argumento[6]))
//dw_2.SetItem(1, "espe_codigo", gi_CodEspecie)

//istr_mant.argumento[3]	=	String(gi_CodEspecie)
//istr_mant.argumento[9]	=	"1"
istr_mant.argumento[10]	=	"0"

istr_mant.argumento[3]	=	''			//	especie
istr_mant.argumento[4]	=	''			//	variedad
istr_mant.argumento[7]	=	''			//	embalaje
istr_mant.argumento[35]	=	''			//	productor
istr_mant.argumento[9]	=	''			//	etiqueta
istr_mant.argumento[36]	=	''			//	calibre
//istr_mant.argumento[10]	=	''			//	cond_codigo
istr_mant.argumento[37]	=	''			//	Numero cajas

dw_2.Object.clie_codigo.Protect	=	1
dw_2.Object.plde_codigo.Protect	=	1
dw_2.Object.paen_numero.Protect	=	1
dw_2.Object.paen_feccon.Protect	=	1
dw_2.Object.espe_codigo.Protect	=	1
dw_2.Object.vari_codigo.Protect	=	1
dw_2.Object.enva_tipoen.Protect	=	1
dw_2.Object.enva_codigo.Protect	=	1
dw_2.Object.paen_concal.Protect	=	1
dw_2.Object.cate_codigo.Protect	=	1
dw_2.Object.etiq_codigo.Protect	=	1
dw_2.Object.dest_codigo.Protect	=	1
dw_2.Object.paen_ccajas.Protect	=	1
dw_2.Object.clie_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.plde_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.paen_numero.BackGround.Color = RGB(166,180,210)
dw_2.Object.paen_feccon.BackGround.Color = RGB(166,180,210)
dw_2.Object.espe_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.vari_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.enva_tipoen.BackGround.Color = RGB(166,180,210)
dw_2.Object.enva_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.paen_concal.BackGround.Color = RGB(166,180,210)
dw_2.Object.cate_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.etiq_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.dest_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.paen_ccajas.BackGround.Color = RGB(166,180,210)

dw_3.Reset()
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

event ue_antesguardar;Long		ll_fila, ll_numero, ll_cajas, ll_total_cajas, ll_productor, ll_Existe
Integer	li_Secuencia, li_cliente, li_planta, li_paen_mixto, li_busca, &
			li_especie, li_variedad, li_etique, li_categ, li_categoria
String	ls_embala, ls_calibre, ls_null, ls_tpen_codigo

ll_total_cajas	=	0
ls_tpen_codigo	=	'1'
ii_enva_tipoen	=	Integer(ls_null) 
ii_enva_codigo	=	Integer(ls_null)

li_Cliente		=	dw_2.Object.clie_codigo[1] 
li_Planta		=	dw_2.Object.plde_codigo[1]

IF dw_1.RowCount() > 0 THEN
	li_categoria	=	dw_1.Object.cate_codigo[1]
	li_etique		=	dw_1.Object.etiq_codigo[1]
	ls_embala		=	dw_1.Object.emba_codigo[1]
END IF

BuscaEnvaseCod(ls_embala)

IF dw_1.RowCount() > 0 THEN
	li_categoria	=	dw_1.Object.cate_codigo[1]
	li_etique		=	dw_1.Object.etiq_codigo[1]
	ls_embala		=	dw_1.Object.emba_codigo[1]
END IF

li_cliente	=	dw_2.Object.clie_codigo[1]
li_planta	=	dw_2.Object.plde_codigo[1]
ll_numero=	dw_2.Object.paen_numero[1]

SELECT	IsNull(Max(pafr_secuen), 0)
	INTO	:li_Secuencia
	FROM	dba.palletfruta
	WHERE	clie_codigo	=	:li_cliente
	AND	plde_codigo =	:li_planta
	AND	paen_numero	=	:ll_numero;

//dw_1.ResetUpdate()
dw_3.ResetUpdate()

DwItemStatus	Estadol

FOR ll_fila	=	1	TO dw_1.RowCount()		
	il_Fila		= dw_1.RowCount()
	
	li_cliente	=	dw_1.GetItemNumber(ll_fila,'clie_codigo')
	li_planta	=	dw_1.GetItemNumber(ll_fila,'plde_codigo')
	li_especie	=	dw_1.GetItemNumber(ll_fila,'espe_codigo')
	li_variedad	=	dw_1.GetItemNumber(ll_fila,'vari_codigo')
	ls_embala	=	dw_1.GetItemString(ll_fila,'emba_codigo')
	li_etique	=	dw_1.GetItemNumber(ll_fila,'etiq_codigo')
	ll_productor=	dw_1.GetItemNumber(ll_fila,'prod_codigo')
	ls_calibre	=	dw_1.GetItemString(ll_fila,'capr_calibr')
	li_categoria=	dw_1.Object.cate_codigo[il_Fila]
	
	IF Isnull(li_etique) THEN li_etique = 1			// pendiente
	IF Isnull(li_categoria) THEN li_categoria	=	1

//	li_busca = dw_3.Find("clie_codigo   = "+String(li_cliente) + &
//							  " AND paen_numero= "+String(ll_numero) + &
//							  " AND espe_codigo= "+String(li_especie) + &
//							  " AND vari_codigo= "+String(li_variedad) +&
//							  " AND emba_codigo= '"+ls_embala + "'"	 +&
//							  " AND prod_codigo= "+String(ll_productor)+ &
//							  " AND etiq_codigo= "+String(li_etique) + &
//							  " AND plde_codigo= "+String(li_planta) + &
//							  " AND pafr_calibr = '"+ ls_calibre+ "'",1,dw_3.RowCount())							    					  	
//							  
//	IF li_busca = 0 THEN
//		li_busca	=	dw_3.InsertRow(0)
//		dw_3.SetItem(li_busca,'clie_codigo',li_cliente)
//		dw_3.SetItem(li_busca,'paen_numero',ll_numero)
//		dw_3.SetItem(li_busca,'espe_codigo',li_especie)
//		dw_3.SetItem(li_busca,'vari_codigo',li_variedad)
//		dw_3.SetItem(li_busca,'emba_codigo',ls_embala)
//		dw_3.SetItem(li_busca,'prod_codigo',ll_productor)
//		dw_3.SetItem(li_busca,'etiq_codigo',li_etique)
//		dw_3.SetItem(li_busca,'plde_codigo',li_planta)		
//		dw_3.SetItem(li_busca,'pafr_calibr',ls_calibre)
//	END IF
//	
//	ll_cajas	= dw_3.GetItemNumber(li_busca,'pafr_ccajas')
//	IF Isnull(ll_cajas) THEN ll_cajas	=	0
//	ll_cajas	=	(ll_cajas + 1)	
//	
//	dw_3.SetItem(li_busca,'frio_tipofr',"1")
//	dw_3.SetItem(li_busca,'prod_codrot',ll_productor)
//	dw_3.SetItem(li_busca,'cocc_codigo',1)
//	dw_3.SetItem(li_busca,'pefr_codigo',1)		
//	dw_3.SetItem(li_busca,'plde_origen',li_planta)
//	dw_3.SetItem(li_busca,'cate_codigo',li_categoria)
//	dw_3.SetItem(li_busca,'tpmv_codigo',4)

//	dw_3.SetItem(li_busca,'lote_codigo',dw_1.Object.capr_nrlote[il_Fila])
//	dw_3.SetItem(li_busca,'pafr_fecemb',dw_1.Object.capr_fecemb[il_Fila])
//	dw_3.SetItem(li_busca,'pafr_estemb',1)
//	////pafr_tipdoc
//	dw_3.SetItem(li_busca, 'pafr_docrel',dw_1.Object.capr_docrel[il_Fila])
//	dw_3.SetItem(li_busca,'pafr_ccajas', ll_cajas)		
//	dw_3.SetItem(li_busca,'mfee_numero',0)
//	dw_3.SetItem(li_busca,'vari_codrot',1)
//	dw_3.SetItem(li_busca,'pafr_copack',dw_1.Object.capr_cespak[il_Fila])
//	dw_3.SetItem(li_busca,'pafr_varrot',dw_1.Object.capr_varrot[il_Fila]) 
//	dw_3.SetItem(li_busca,'pafr_huert1',dw_1.Object.prod_predio[il_Fila])
//	dw_3.SetItem(li_busca,'pafr_cuart1',dw_1.Object.prod_cuarte[il_Fila])	
///	dw_1.SetItem(ll_fila,'capr_numpal', ll_numero)
	////dw_1.SetItem(ll_fila,'capr_estado', 1)	
	
	ll_total_cajas++
	
	Estadol	=	dw_1.GetItemStatus(ll_fila, 0, Primary!)
	//IF dw_1.GetItemStatus(ll_fila,0,Primary!) = New! OR dw_1.GetItemStatus(ll_fila,0,Primary!) = NewModified! THEN	
	IF IsNull(dw_1.Object.capr_numpal[ll_fila]) OR dw_1.Object.capr_numpal[ll_fila] = 0 THEN
		li_busca	=	dw_3.InsertRow(0)
		dw_3.SetItem(li_busca,'clie_codigo',li_cliente)
		dw_3.SetItem(li_busca,'paen_numero',ll_numero)
		dw_3.SetItem(li_busca,'espe_codigo',li_especie)
		dw_3.SetItem(li_busca,'vari_codigo',li_variedad)
		dw_3.SetItem(li_busca,'emba_codigo',ls_embala)
		dw_3.SetItem(li_busca,'prod_codigo',ll_productor)
		dw_3.SetItem(li_busca,'etiq_codigo',li_etique)
		dw_3.SetItem(li_busca,'plde_codigo',li_planta)		
		dw_3.SetItem(li_busca,'pafr_calibr',ls_calibre)
		dw_3.SetItem(li_busca,'frio_tipofr',"1")
		dw_3.SetItem(li_busca,'prod_codrot',ll_productor)		
		dw_3.SetItem(li_busca,'cocc_codigo',1)
		dw_3.SetItem(li_busca,'pefr_codigo',1)				
		dw_3.SetItem(li_busca,'plde_origen',li_planta)		
		dw_3.SetItem(li_busca,'cate_codigo',li_categoria)
		dw_3.SetItem(li_busca,'tpmv_codigo',4)		
		dw_3.SetItem(li_busca,'pafr_fecemb',dw_1.Object.capr_fecemb[ll_fila])		
		dw_3.SetItem(li_busca,'pafr_estemb',1)		
		dw_3.SetItem(li_busca,'pafr_docrel',dw_1.Object.capr_docrel[ll_fila])		
		dw_3.SetItem(li_busca,'pafr_ccajas',1)		
		dw_3.SetItem(li_busca,'mfee_numero',0)		
		dw_3.SetItem(li_busca,'vari_codrot',dw_1.Object.capr_varrot[ll_fila])		
		dw_3.SetItem(li_busca,'pafr_copack',dw_1.Object.capr_cespak[ll_fila])
		dw_3.SetItem(li_busca,'pafr_varrot',dw_1.Object.capr_varrot[ll_fila]) 
		dw_3.SetItem(li_busca,'pafr_huert1',dw_1.Object.prod_predio[ll_fila])
		dw_3.SetItem(li_busca,'pafr_cuart1',dw_1.Object.prod_cuarte[ll_fila])
		
		dw_1.SetItem(ll_fila,'capr_numpal', ll_numero)
		dw_3.SetItem(li_busca,'pafr_secuen',dw_1.Object.capr_numero[ll_fila])	
	END IF		
NEXT
 
FOR ll_fila = 1 TO dw_1.RowCount()	
	dw_1.SetItemStatus(ll_fila,'capr_numpal',Primary!,DataModified!)
	////dw_1.SetItemStatus(ll_fila,'capr_estado',Primary!,DataModified!)
NEXT

IF dw_3.RowCount() > 0 AND Isnull(dw_2.Object.espe_codigo[1]) AND &
	Isnull(dw_2.Object.vari_codigo[1]) THEN
	////dw_2.SetItem(1,'paen_tipopa',)
	
	dw_2.SetItem(1,'clie_codigo', dw_1.GetItemNumber(1,'clie_codigo'))
	dw_2.SetItem(1,'plde_codigo', dw_1.GetItemNumber(1,'plde_codigo'))
	dw_2.SetItem(1,'paen_numero', ll_numero)	
	dw_2.SetItem(1,'espe_codigo', dw_1.GetItemNumber(1,'espe_codigo'))
	dw_2.SetItem(1,'vari_codigo', dw_1.GetItemNumber(1,'vari_codigo'))	
	li_categ	=	dw_1.GetItemNumber(1,'cate_codigo')
	IF IsNull(li_categ) THEN li_categ	=	1
	dw_2.SetItem(1,'cate_codigo',	li_categ) 	
	dw_2.SetItem(1,'cama_codigo',0)	
	dw_2.SetItem(1,'tpen_codigo',"1")	
	dw_2.SetItem(1,'frio_tipofr',"1")
	li_etique		=	dw_1.GetItemNumber(1,'etiq_codigo')
	IF Isnull(li_etique) THEN li_etique = 1			// pendiente
	dw_2.SetItem(1,'etiq_codigo',   li_etique)
	dw_2.SetItem(1,'paen_estado', 1)
	dw_2.SetItem(1,'paen_feccon', dw_1.GetItemDate(1,'capr_fecemb'))
	////dw_2.Object.paen_altura[1]	= 
	dw_2.SetItem(1,'paen_fumiga',0)
	dw_2.SetItem(1,'paen_inspec',0)	
	dw_2.SetItem(1,'sepl_codigo',1)
	dw_2.Object.enva_tipoen[1]	=	ii_enva_tipoen 
	dw_2.Object.enva_codigo[1]	=	ii_enva_codigo		
	dw_2.SetItem(1,'dest_codigo',999)
END IF

IF dw_3.RowCount() >= 0 AND dw_2.RowCount() <> 0  THEN
	dw_2.SetItem(1,'paen_ccajas',ll_total_cajas)
END IF
dw_1.AcceptText()
dw_2.AcceptText()
dw_3.AcceptText()

end event

event ue_modifica_detalle();Cuentacajas()

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

//// Borra detalle spro_palletfruta, del numero pallet ya que se rehace de nuevo
//IF  wf_actualiza_db(True)  THEN
//	lb_borrado	=	True
//END IF

//IF lb_borrado AND wf_actualiza_db(False) THEN
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

type dw_1 from w_mant_encab_deta_csd`dw_1 within w_maed_generapalletdesdecajas_granel
integer x = 142
integer width = 2834
integer height = 1136
integer taborder = 100
string title = "Detalle de Pallets"
string dataobject = "dw_mues_cajasprodpallet"
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

type dw_2 from w_mant_encab_deta_csd`dw_2 within w_maed_generapalletdesdecajas_granel
integer x = 73
integer y = 56
integer width = 2985
integer height = 812
string dataobject = "dw_mant_spro_palletencab_cajas"
end type

event dw_2::itemchanged;call super::itemchanged;Long		ll_null, ll_PalletMan, ll_Data, ll_NroPallet
String	ls_columna, ls_asda, ls_Nula
Date		ld_Nula
Integer	li_FilaMan, li_Cliente

DataWIndowChild	dw_calibres

SetNull(ll_null)
SetNull(ld_nula)
SetNull(ls_Nula)

ls_columna = GetColumnName()

ii_yaexiste	=	0

CHOOSE CASE ls_columna
	CASE "paen_palneo"
		IF Len(Data) < 5  THEN
			MessageBox("Error", "Ha leído un Código~rde Barra Inválido.", &
							StopSign!)
			This.SetItem(il_Fila, ls_Columna, ls_Nula)
			RETURN 1
			
		END IF
		
		IF Len(data) > 8 THEN
			li_Cliente		=	Integer(Mid(Data, 1, Len(Data) - 7))
			ll_NroPallet	=	Long(Mid(Data, Len(Data) - 6))
			
			IF li_Cliente <> Integer(istr_mant.argumento[1]) THEN
			   Messagebox("Cuidado", "Código de Cliente Distinto a Encabezado")
				This.SetItem(il_Fila, ls_Columna, ls_Nula)
				RETURN 1				
			ELSE
				dw_2.Object.paen_tipopa[1]	=	1
			END IF
		ELSE
			li_Cliente		=	0
			ll_NroPallet	=	Long(Data)
			dw_2.Object.paen_tipopa[1]	=	2	
		END IF

		IF li_Cliente <> Integer(istr_mant.argumento[1]) THEN
			MessageBox("Error", "Ha leído un Código~rde Barra Inválido.", &
							StopSign!)
			This.SetItem(il_Fila, ls_Columna, ls_Nula)
			RETURN 1
		END IF
		
		dw_2.Object.paen_numero[1] = 	ll_NroPallet
		istr_mant.argumento[2]		=	String(ll_NroPallet)
		
		IF ExistePallet(String(ll_NroPallet)) OR IsNull(ll_NroPallet) THEN
			ii_yaexiste = 1
			dw_2.SetItem(1, "paen_numero", ll_null)
			RETURN 1
		END IF

		This.SetItem(il_Fila,"paen_numero",ll_NroPallet)
				
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
END CHOOSE



HabilitaIngreso()
end event

event dw_2::clicked;call super::clicked;IF dw_2.Object.paen_pmixto[1]	=	1 AND Integer(dw_2.Object.paen_pmixto.Protect)	=	1 THEN RETURN

CHOOSE CASE dwo.name
		
	CASE "buscavariedad"
		buscavariedad()
		
//	CASE "buscaembalaje"
//		buscaembalaje()
	CASE "buscaenvase"
		BuscaEnvase()
END CHOOSE
end event

event dw_2::doubleclicked;//
end event

type pb_nuevo from w_mant_encab_deta_csd`pb_nuevo within w_maed_generapalletdesdecajas_granel
integer y = 252
end type

type pb_eliminar from w_mant_encab_deta_csd`pb_eliminar within w_maed_generapalletdesdecajas_granel
boolean visible = false
integer y = 480
end type

type pb_grabar from w_mant_encab_deta_csd`pb_grabar within w_maed_generapalletdesdecajas_granel
boolean visible = false
integer y = 688
end type

type pb_imprimir from w_mant_encab_deta_csd`pb_imprimir within w_maed_generapalletdesdecajas_granel
integer y = 896
end type

type pb_salir from w_mant_encab_deta_csd`pb_salir within w_maed_generapalletdesdecajas_granel
integer y = 1156
end type

type pb_ins_det from w_mant_encab_deta_csd`pb_ins_det within w_maed_generapalletdesdecajas_granel
integer x = 3296
integer y = 1484
end type

type pb_eli_det from w_mant_encab_deta_csd`pb_eli_det within w_maed_generapalletdesdecajas_granel
boolean visible = false
integer x = 3301
integer y = 1660
end type

type pb_buscar from w_mant_encab_deta_csd`pb_buscar within w_maed_generapalletdesdecajas_granel
boolean visible = false
integer y = 76
end type

type dw_3 from datawindow within w_maed_generapalletdesdecajas_granel
boolean visible = false
integer x = 219
integer y = 1584
integer width = 2565
integer height = 316
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_palletfruta"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event sqlpreview;DwItemStatus	Estado

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

