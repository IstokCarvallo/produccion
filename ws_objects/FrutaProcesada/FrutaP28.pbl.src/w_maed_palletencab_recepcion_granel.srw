$PBExportHeader$w_maed_palletencab_recepcion_granel.srw
forward
global type w_maed_palletencab_recepcion_granel from w_maed_palletencab
end type
type dw_4 from datawindow within w_maed_palletencab_recepcion_granel
end type
type dw_3 from datawindow within w_maed_palletencab_recepcion_granel
end type
type dw_5 from datawindow within w_maed_palletencab_recepcion_granel
end type
end forward

global type w_maed_palletencab_recepcion_granel from w_maed_palletencab
integer x = 14
integer y = 336
integer width = 3461
integer height = 2516
boolean titlebar = true
string title = "Ingreso Pallet Nuevo"
boolean controlmenu = true
boolean border = true
windowtype windowtype = response!
dw_4 dw_4
dw_3 dw_3
dw_5 dw_5
end type
global w_maed_palletencab_recepcion_granel w_maed_palletencab_recepcion_granel

type variables
Str_mant	istr_mant2

DataWindowChild	idwc_categorias, idwc_status, idwc_condicion, &
						idwc_tratamiento, idwc_tipofrio, idwc_destino
										  
Boolean		lb_MensPallet=False, lb_MensPucho=False
Integer		ii_CantPallets, ii_CantPuchos, ii_tpemccajas;

/*================================================================
Vector que guarda datos sobre la ubicacion de un pallet, el vector 
se compondrá por la sgte asignacion:
i_vec_ubicacion[1]=camara, i_vec_ubicacion[2]=calle, 
i_vec_ubicacion[3]=base,   i_vec_ubicacion[4]=posicion,
i_vec_ubicacion[5]=tipo camara
================================================================*/
integer 		i_vec_ubicacion[5]
 

end variables

forward prototypes
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean duplicadoinspecdet (long nroinspeccion)
public function boolean duplicadoinspeccion (long nroinspeccion)
public subroutine buscavariedad ()
public function boolean existe_recprefriopda (integer li_clie_codigo, integer li_plde_codigo, long ll_paen_numero)
public subroutine elimina_recprefriopda (integer li_clie_codigo, integer li_plde_codigo, long ll_paen_numero)
public subroutine existeotraplanta (long al_pallet)
public function boolean comparacajas (string as_tpem_codigo)
public function boolean cargapallet (integer ai_cliente, integer ai_planta, long al_nropallet)
public function boolean duplicado (integer ai_cliente, long al_numero)
public function boolean existecapturapallet (integer ai_cliente, integer ai_planta, long al_pallet)
end prototypes

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF borrando THEN
ELSE
	IF dw_2.Update(True, False) = 1 THEN
		IF dw_1.Update(True, False) = 1 THEN
			IF dw_4.Update(True, False) = 1 THEN 
				Commit;
				
				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
					
					RollBack;
				ELSE
					lb_Retorno	=	True
					
					dw_1.ResetUpdate()
					dw_3.ResetUpdate()
					dw_4.ResetUpdate()
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




//if not dw_2.uf_check_required(0) then return false
//if not dw_1.uf_validate(0) then return false
//if borrando then
//	if dw_1.Update() = 1 and dw_2.Update() = 1 then
//		commit;
//		if sqlca.sqlcode <> 0 then
//				f_errorbasedatos(sqlca,"Lista la tabla")
//			return false
//		else
//			return true
//		end if 
//	else
//		rollback;
//		if sqlca.sqlcode <> 0 then f_errorbasedatos(sqlca,"Lista la tabla")
//		return false
//	end if
//else
//	if dw_2.update() = 1 and dw_1.Update() = 1 and dw_4.Update() = 1 then 
//		commit;
//		if sqlca.sqlcode <> 0 then
//			f_errorbasedatos(sqlca,"Lista la tabla")
//			return false
//		else
//			return true
//		end if 
//	else
//		rollback;
//		if sqlca.sqlcode <> 0 then f_errorbasedatos(sqlca,"Lista la tabla")
//		return false
//	end if
//end if

return true
end function

public function boolean duplicadoinspecdet (long nroinspeccion);Long		ll_fila, ll_numero
Integer  li_planta,li_cliente,li_tipoin, li_secuen
Date		ld_fechai

SetNull(li_secuen)

li_tipoin	=	1
li_planta	= dw_2.Object.plde_codigo[1]
li_cliente	= dw_2.Object.clie_codigo[1]
ll_numero   = dw_2.Object.paen_numero[1]

ll_fila	= dw_4.Find("inpe_tipoin = " + String(li_tipoin) + " AND inpe_numero = " + String(nroinspeccion) + &
               " AND paen_numero = " + String(ll_numero) + &
               " AND plde_codigo = " + String(li_planta) + " AND clie_codigo = " + String(li_cliente), &
					1, dw_4.RowCount())

IF ll_fila > 0 THEN
	RETURN True
ELSE

  SELECT inpe_secuen INTO :li_secuen
	FROM dba.INSPECPALDET
	WHERE inpe_tipoin = :li_tipoin
	AND   inpe_numero = :nroinspeccion
	AND   clie_codigo = :li_cliente
	AND   plde_codigo = :li_planta
	AND   paen_numero = :ll_numero;
	
	IF sqlca.SQLCode = -1 THEN
		RETURN True
	ELSEIF Isnull(li_secuen) THEN
		    RETURN False
		ELSE
			 RETURN True
		END IF
   END IF

end function

public function boolean duplicadoinspeccion (long nroinspeccion);//Long		ll_fila
//Integer  li_planta,li_cliente,li_tipoin,li_secuen
//Date		ld_fechai
//
//SetNull(li_secuen)
//
//li_tipoin	=	1
//li_planta	= dw_2.Object.plde_codigo[1]
//li_cliente	= dw_2.Object.clie_codigo[1]
//
//ll_fila	= dw_5.Find("inpe_tipoin = " + String(li_tipoin) + " AND inpe_numero = " + String(nroinspeccion) + &
//               " AND plde_codigo = " + String(li_planta) + " AND clie_codigo = " + String(li_cliente), &
//					1, dw_5.RowCount())
//
//IF ll_fila > 0 THEN
//	RETURN True
//ELSE
//
//	SELECT inpe_secuen INTO :li_secuen
//	FROM dba.INSPECPALENC
//	WHERE inpe_tipoin = :li_tipoin
//	AND   inpe_numero = :nroinspeccion
//	AND   clie_codigo = :li_cliente
//	AND   plde_codigo = :li_planta;
//	
//	IF sqlca.SQLCode = -1 THEN
//		RETURN True
//	ELSEIF Isnull(li_secuen) THEN
//		    RETURN False
//		ELSE
			 RETURN True
//		END IF
//   END IF
//
end function

public subroutine buscavariedad ();//Str_busqueda	lstr_busq
//
//dw_2.Modify("buscavariedad.border = 0")
//dw_2.Modify("buscavariedad.border = 5")
//
//lstr_busq.argum[1]	=	istr_mant.Argumento[1]
//lstr_busq.argum[2]	=	String(dw_2.GetItemNumber(1, "espe_codigo"))
//
//OpenWithParm(w_busc_variedades, lstr_busq)
//
//lstr_busq	= Message.PowerObjectParm
//
//IF lstr_busq.argum[4] <> '' THEN
//	istr_mant.argumento[4]	=	lstr_busq.argum[4]
//	istr_mant.argumento[5]	=	lstr_busq.argum[5]
//	
//	dw_2.setItem(1, "vari_codigo", Integer(lstr_busq.argum[4]))
//	dw_2.setItem(1, "vari_nombre", lstr_busq.argum[5])
//	dw_2.SetColumn("vari_codigo")
//	dw_2.SetFocus()
//END IF
//
//dw_2.Modify("buscavariedad.border = 0")
//dw_2.Modify("buscavariedad.border = 6")
end subroutine

public function boolean existe_recprefriopda (integer li_clie_codigo, integer li_plde_codigo, long ll_paen_numero);Integer	li_Nulo

SetNull(li_Nulo)

i_vec_ubicacion[1]	=	li_Nulo
i_vec_ubicacion[2]	=	li_Nulo
i_vec_ubicacion[3]	=	li_Nulo
i_vec_ubicacion[4]	=	li_Nulo
i_vec_ubicacion[5]	=	li_Nulo

Select CAMA_CODIGO,PAEN_CALLE,PAEN_BASE,PAEN_POSICI INTO 
	:i_vec_ubicacion[1],:i_vec_ubicacion[2],:i_vec_ubicacion[3],:i_vec_ubicacion[4] FROM 
	DBA.PALLETENCAB WHERE
	clie_codigo= :li_clie_codigo AND 
	plde_codigo= :li_plde_codigo AND
	paen_numero= :ll_paen_numero;
	/*control SQl*/
	
	
//verifica si encontró registro en la DBA.RECPREFRIOPDA
if isnull(i_vec_ubicacion[1]) then
	return FALSE
else
	
	Select cama_tipoca into 
	:i_vec_ubicacion[5] from
	dba.camarasbode where
	plde_codigo= :li_plde_codigo AND
	cama_codigo= :i_vec_ubicacion[1];
	/*control SQl*/
	return TRUE
end if	
	
end function

public subroutine elimina_recprefriopda (integer li_clie_codigo, integer li_plde_codigo, long ll_paen_numero);//
DELETE FROM dba.recprefriopda WHERE
	clie_codigo= :li_clie_codigo AND 
	plde_codigo= :li_plde_codigo AND
	paen_numero= :ll_paen_numero;
	
	/*control SQl*/


end subroutine

public subroutine existeotraplanta (long al_pallet);
Integer	li_Cliente, li_Planta, li_Estado

li_Cliente	=	dw_2.Object.clie_codigo[1]
li_Planta	=	dw_2.Object.plde_codigo[1]

SELECT COUNT(*)
INTO :li_Estado
FROM dba.palletencab
WHERE clie_codigo = :li_Cliente
AND   plde_codigo <> :li_Planta
AND   paen_numero = :al_Pallet
AND   paen_estado = 1;

IF li_Estado > 0 THEN
	MessageBox("Cuidado","Este Pallet Está en Existencia en Otra Planta")
END IF

RETURN


end subroutine

public function boolean comparacajas (string as_tpem_codigo);Integer 	li_tpemcodigo, li_cliente, li_paenccajas
String	ls_embalaje

li_cliente 		= 	dw_2.Object.clie_codigo[1]
ls_embalaje		=	dw_2.Object.emba_codigo[1]
li_paenccajas	=	dw_2.Object.paen_ccajas[1]

IF IsNull(li_paenccajas) THEN li_paenccajas = 0

li_tpemcodigo	=	Integer(as_tpem_codigo)

SELECT tpem_cancaj
INTO :ii_tpemccajas
FROM dba.TipoPallEmba
WHERE clie_codigo =	:li_cliente 	and
	emba_codigo 	=	:ls_embalaje	and
	tpem_codigo 	=	:li_tpemcodigo;
	
IF IsNull(ii_tpemccajas) or ii_tpemccajas = 0 THEN
	Return False
ELSEIF ii_tpemccajas <> li_paenccajas THEN
	Return False
ELSE
	Return True
END IF
end function

public function boolean cargapallet (integer ai_cliente, integer ai_planta, long al_nropallet);Boolean lb_Retorno
Integer li_Fila_E, li_Fila_D, li_Fila_Ins, li_Fila_C
String  ls_Especie, ls_Variedad

lb_Retorno	=	False

li_Fila_E	=	dw_4.Retrieve(ai_Cliente,al_Nropallet,ai_Planta)

IF li_Fila_E > 0 THEN
			
	li_Fila_D = dw_5.Retrieve(ai_Cliente, al_Nropallet, ai_Planta)
	IF li_Fila_D > 0 THEN		
		dw_2.SetItem(il_Fila, "clie_codigo", dw_4.Object.clie_codigo[1])
		dw_2.SetItem(il_Fila, "paen_numero", dw_4.Object.paen_numero[1])
		dw_2.SetItem(il_Fila, "plde_codigo", dw_4.Object.plde_codigo[1])
		dw_2.SetItem(il_Fila, "paen_ccajas", dw_4.Object.paen_ccajas[1])
		dw_2.SetItem(il_Fila, "copa_codigo", dw_4.Object.copa_codigo[1])
		dw_2.SetItem(il_Fila, "paen_fecemb", dw_4.Object.paen_feccon[1])
		dw_2.SetItem(il_Fila, "etiq_codigo", dw_4.Object.etiq_codigo[1])
		dw_2.SetItem(il_Fila, "cama_codigo", dw_4.Object.cama_codigo[1])
		dw_2.SetItem(il_Fila, "paen_tipopa", dw_4.Object.paen_tipopa[1])		
		dw_2.SetItem(il_Fila, "espe_codigo", dw_4.Object.espe_codigo[1])
		dw_2.SetItem(il_Fila, "vari_codigo", dw_4.Object.vari_codigo[1])
		dw_2.SetItem(il_Fila, "cate_codigo", dw_4.Object.cate_codigo[1])
		dw_2.SetItem(il_Fila, "paen_estado", 1)
		dw_2.SetItem(il_Fila, "paen_calle",  1)
		dw_2.SetItem(il_Fila, "paen_base", 	 1)
		dw_2.SetItem(il_Fila, "paen_posici", 1)				
		dw_2.SetItem(il_Fila, "tmvp_codigo", 1)				
		dw_2.SetItem(il_Fila, "stat_codigo", 1)
		dw_2.SetItem(il_Fila, "cond_codigo", 0)
		dw_2.SetItem(il_Fila, "trat_codigo", 2)
		dw_2.SetItem(il_Fila, "dest_codigo", 999)
		dw_2.SetItem(il_Fila, "paen_inspec", 0)
		dw_2.SetItem(il_Fila, "frio_codigo", '1')
		dw_2.SetItem(il_Fila, "paen_pcopda", 2)
		
		ls_Especie	=	String(dw_4.Object.espe_codigo[1],'00')
		ls_Variedad	=	String(dw_4.Object.vari_codigo[1],'0000')
		
		IF dw_4.Object.paen_tipopa[1] = 1 THEN
			dw_2.Object.tpem_codigo.Protect	=	0
			dw_2.Object.tpem_codigo.BackGround.Color = RGB(255,255,255)
		ELSE
			dw_2.Object.tpem_codigo.Protect	=	1
			dw_2.Object.tpem_codigo.BackGround.Color = RGB(166,180,210)			
		END IF		
		
		FOR li_Fila_D	= 1 TO dw_5.Rowcount()  
			li_Fila_Ins = 	dw_1.InsertRow(0)
			IF li_Fila_D = 1 THEN
				dw_2.SetItem(il_Fila, "emba_codigo", dw_5.Object.emba_codigo[1])
				dw_2.GetChild("tpem_codigo", dw_emba)
				dw_emba.SetTransObject(sqlca)
				dw_emba.Retrieve(Integer(istr_mant.argumento[1]), dw_5.Object.emba_codigo[1])

				dw_2.SetItem(il_Fila, "prod_codigo", dw_5.Object.prod_codigo[1])
				dw_2.SetItem(il_Fila, "paen_calibr", dw_5.Object.pafr_calibr[1])
			END IF
			
			IF li_Fila_Ins > 0 THEN
				dw_1.SetItem(li_Fila_Ins,"clie_codigo",dw_5.Object.clie_codigo[li_Fila_D])
				dw_1.SetItem(li_Fila_Ins,"paen_numero",dw_5.Object.paen_numero[li_Fila_D])
				dw_1.SetItem(li_Fila_Ins,"espe_codigo",dw_5.Object.espe_codigo[li_Fila_D])
				dw_1.SetItem(li_Fila_Ins,"vari_codigo",dw_5.Object.vari_codigo[li_Fila_D])								
				dw_1.SetItem(li_Fila_Ins,"emba_codigo",dw_5.Object.emba_codigo[li_Fila_D])
				dw_1.SetItem(li_Fila_Ins,"prod_codigo",dw_5.Object.prod_codigo[li_Fila_D])
				dw_1.SetItem(li_Fila_Ins,"cond_codigo",0)
				dw_1.SetItem(li_Fila_Ins,"etiq_codigo",dw_5.Object.etiq_codigo[li_Fila_D])
				dw_1.SetItem(li_Fila_Ins,"plde_codigo",dw_5.Object.plde_codigo[li_Fila_D])
				dw_1.SetItem(li_Fila_Ins,"pafr_calibr",dw_5.Object.pafr_calibr[li_Fila_D])
				dw_1.SetItem(li_Fila_Ins,"pafr_secuen",dw_5.Object.pafr_secuen[li_Fila_D])
				//dw_1.SetItem(li_Fila_Ins,"pafr_ccajas",dw_5.Object.ccajas[li_Fila_D])
				dw_1.SetItem(li_Fila_Ins,"pafr_ccajas",dw_5.Object.pafr_ccajas[li_Fila_D])
				//dw_1.SetItem(li_Fila_Ins,"pafr_nrlote",dw_5.Object.lote_codigo[li_Fila_D])
				dw_1.SetItem(li_Fila_Ins,"pafr_copack",dw_5.Object.pafr_copack[li_Fila_D])
				dw_1.SetItem(li_Fila_Ins,"pafr_varrot",dw_5.Object.pafr_varrot[li_Fila_D])
				dw_1.SetItem(li_Fila_Ins,"pafr_huert1",dw_5.Object.pafr_huert1[li_Fila_D])
				dw_1.SetItem(li_Fila_Ins,"pafr_cuart1",dw_5.Object.pafr_cuart1[li_fila_D])
				dw_1.SetItem(li_Fila_Ins,"pafr_fecemb",dw_5.Object.pafr_fecemb[li_Fila_D])
				dw_1.SetItem(li_Fila_Ins,"pafr_fecing",Date(istr_mant.Argumento[36]))
			END IF				
		NEXT		
		lb_Retorno = True
	END IF
END IF

RETURN lb_Retorno

end function

public function boolean duplicado (integer ai_cliente, long al_numero);Long		ll_Fila


ll_Fila	=	dw_3.Find("clie_codigo = " + String(ai_Cliente) + &
						 	 " AND plde_codigo = " + istr_mant.Argumento[6] + &
							 " AND paen_numero = " + String(al_Numero) , 1, &
							 dw_3.RowCount())
	
IF ll_Fila > 0  THEN
	MessageBox("Error","Pallet ya fue incluido en Detalle de Recepción", &
					Information!, Ok!)
						
	RETURN True
END IF

RETURN False


//
//Long		ll_fila
//String	ls_codigo,ls_cliente,ls_numero
//
////ls_codigo	= String(dw_2.Object.plde_codigo[1])
//ls_codigo	=	istr_mant2.Argumento[1]
//ls_numero	= 	istr_mant2.Argumento[2]
//ls_cliente	= 	String(dw_2.Object.clie_codigo[1])
//
//IF istr_mant2.Argumento[5] = 'dw_mues_repalletdeta' THEN
//	ll_fila	= dw_3.Find("plde_codigo = " + ls_codigo + " AND clie_codigo = " + ls_cliente + " AND paen_numero =	" + campo, 1, dw_3.RowCount())
//ELSE
//	ll_fila	= dw_3.Find("plde_codigo = " + ls_codigo + " AND clie_codigo = " + ls_cliente + " AND paen_numero =	" + campo, 1, dw_3.RowCount())
//END IF
//
//IF ll_fila > 0 THEN
//	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
//	RETURN True
//ELSE
//	RETURN False
//END IF
end function

public function boolean existecapturapallet (integer ai_cliente, integer ai_planta, long al_pallet);Integer		li_Existe

SELECT Count(*)
INTO  :li_Existe
FROM  dba.palletencab
WHERE clie_codigo = :ai_Cliente
AND   plde_codigo = :ai_Planta
AND   paen_numero = :al_Pallet;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Palletencab")
	RETURN False
ELSEIF Not IsNull(li_Existe) AND li_Existe > 0 THEN
	MessageBox("Error","Pallet Ya Existe", &
					Information!, Ok!)
					
	RETURN False
END IF

RETURN True
end function

on w_maed_palletencab_recepcion_granel.create
int iCurrent
call super::create
this.dw_4=create dw_4
this.dw_3=create dw_3
this.dw_5=create dw_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_4
this.Control[iCurrent+2]=this.dw_3
this.Control[iCurrent+3]=this.dw_5
end on

on w_maed_palletencab_recepcion_granel.destroy
call super::destroy
destroy(this.dw_4)
destroy(this.dw_3)
destroy(this.dw_5)
end on

event ue_antesguardar;Integer	li_Contador, li_empesp, li_empvar, &
			li_expo, li_PFrut, li_Especie, li_Variedad
String	ls_Mensaje, ls_Columna[], ls_empemb, ls_Nombre
Long		ll_Fila_dw_3
Date	  	ld_pafr_fecemb

IF IsNull(dw_1.Object.paen_numero[il_Fila]) OR &
	dw_1.Object.paen_numero[il_Fila] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nNúmero de Pallet"
	ls_Columna[li_Contador]	=	"paen_numero"
END IF

IF li_Contador > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + &
					ls_Mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_Columna[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF

IF Not IsNull(dw_1.Object.paen_numero[il_Fila]) AND &
	dw_1.Object.paen_numero[il_Fila] <> 0 AND & 
	dw_4.RowCount() > 0 AND dw_5.RowCount() > 0 THEN
	
	// Bloquea Grabación
	//Integer li_expo
	SELECT expo_codigo INTO :li_expo
	FROM dba.parempresa;
	UPDATE dba.parempresa SET
	expo_codigo = li_expo;		
		
	ll_Fila_dw_3 = dw_3.InsertRow(0)
	
	dw_3.Object.plde_codigo[ll_Fila_dw_3]	=	Integer(istr_mant.argumento[6])
	dw_3.Object.clie_codigo[ll_Fila_dw_3]	=	Integer(istr_mant.argumento[1])
	dw_3.Object.rfpe_numero[ll_Fila_dw_3]	=	Long(istr_mant2.argumento[2])
	dw_3.Object.rfpe_pcopda[ll_Fila_dw_3]	=  2
	dw_3.Object.paen_numero[ll_Fila_dw_3]  =  dw_2.Object.paen_numero[1]
	
	dw_3.Object.paen_tipopa[ll_Fila_dw_3]	=	dw_2.Object.paen_tipopa[1]
	dw_3.Object.espe_codigo[ll_Fila_dw_3]	=	dw_2.Object.espe_codigo[1]
	dw_3.Object.vari_codigo[ll_Fila_dw_3]	=	dw_2.Object.vari_codigo[1]
	dw_3.Object.emba_codigo[ll_Fila_dw_3]	=	dw_2.Object.emba_codigo[1]
	dw_3.Object.prod_codigo[ll_Fila_dw_3]	=	dw_2.Object.prod_codigo[1]
	dw_3.Object.paen_calibr[ll_Fila_dw_3]	=	dw_2.Object.paen_calibr[1]
	dw_3.Object.paen_ccajas[ll_Fila_dw_3]	=	dw_2.Object.paen_ccajas[1]
	
	IF NOT Isnull(dw_3.Object.vari_codigo[ll_Fila_dw_3]) THEN
		li_Especie	=	dw_2.Object.espe_codigo[1]
		li_Variedad	=	dw_2.Object.vari_codigo[1]

		SELECT vari_nombre
		INTO   :ls_Nombre
		FROM dba.variedades
		WHERE espe_codigo = :li_Especie
		AND   vari_codigo = :li_Variedad;
		
		IF NOT IsNull(ls_Nombre) OR ls_Nombre = '' THEN
			dw_3.Object.vari_nombre[ll_Fila_dw_3]	=	ls_Nombre
		END IF
	END IF
			
	IF ISNull(dw_4.Object.cama_codigo[il_Fila]) THEN	
		dw_1.Object.cama_codigo[il_Fila]		=	0
		dw_1.Object.paen_calle[il_Fila]		=	1
		dw_1.Object.paen_base[il_Fila]		=	1
		dw_1.Object.paen_posici[il_Fila]		=	1
		dw_1.Object.tmvp_codigo[il_Fila]		=	0	
	END IF
	
	dw_4.ResetUpdate() 
	IF dw_4.RowCount() > 0 THEN
		dw_4.SetItem(1,'paen_estado', 2)
		dw_4.SetItemStatus(1,'paen_estado',Primary!,DataModified!)
	END IF
END IF



end event

event open;/*
	Argumentos de istr_mant => Envío a Mantención de Detalle
		Argumento	[1]	=	Código de Exportador
						[2]	=	Número de Pallet
						[3]	=	Código de Especie
						[4]	=	Código de Variedad
						[5]	=	Nombre de Variedad
						[6]	=	Código de Planta
						[7]	=	Código de Embalaje
						[8]	=	Nombre del Embalaje
						[9]	=	Código de Etiqueta
						[10]	=	Código de Condición
						[11]	=	
						[12]	=	
						[13]	=	Nombre de Condición
						[14]	=	Nombre de Planta
						[15]	=	Lista de Productores de Palltes
						[16]	=	Condición de Sólo Consulta (0 = No / 1 = Si)
						[20]	=	Tipo de Recepción : 1 Ingreso desde Packing => Recfruprocee_particular	
						[21]	=	Packing Origen
						[22]	=	Pallet de Exportación ( 1	=	Si	/	0	=	No
						[23]	=	Pallet de Mixto ( 1	=	Si	/	0	=	No
						[25]	=	Inspeccionado ( 1	=	Si	/	0	=	No
						[26]	=	Destino
						[27]  =  Lista de Variedades
						[28]	=  Fecha Embalaje
						[31]  =  Tipo de Recepción: 1=Packing, -1=Otro
						[32]  =  Nº asda
						[33]  =  Recepción Transmitida
						[39]	= 	Fecha Ingreso
						

	Argumentos de istr_mant2 => Recepción por Ingreso o Consulta
		Argumento	[1]	=	Código de Planta
						[2]	=	Número de Folio Recepción
						[3]	=	Código de Exportador
						[4]	=	
						[5]	=	Tipo de packing
						[6]	=	Número de Pallet
						[7]	=	Condición de Sólo Consulta (0 = No / 1 = Si)
						[8]	=	
						[9]	=	Código de Especie
						[10]	=	Código de Variedad
						[11]	=	Tarjas Definitivas
						[12]	=	Cantidad de Tarjas Transitorias
						[13]	=	Nombre de Condición
						[14]	=	Nombre de Planta
						[15]	=	Lista de Productores de pallets
						[16]	=	Condición de Sólo Consulta (0 = No / 1 = Si)
						[20]	=	Tipo de Recepción : 1 Ingreso desde Packing => Recfruprocee_particular	
						[21]	=	Packing Origen
						[22]  =  Lista de Variedades (repaletizaje)
						[23]	=  fecha Embalaje Menor (repaletizaje)
						[31]  =  Tipo de Recepción: 1=Packing, -1=Otro
						
*/
X	=	10
Y	=	300
This.Height	= 2020
im_menu		= m_principal

Integer	li_cliente, li_planta
String	ls_Embalaje

This.Icon										=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_2.Object.clie_codigo.Protect	=	1
dw_2.Object.plde_codigo.Protect	=	1
dw_2.Object.clie_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.plde_codigo.BackGround.Color = RGB(166,180,210)

/* La asignación a las siguientes variables no se hereda, debe ser adicional en ventana descendiente */
istr_mant2.Argumento[20]	=	'0'
istr_mant2.Argumento[21]	=	''
istr_mant2.Argumento[25]	=	'0'
istr_mant2.Argumento[26]	=	'0'

istr_mant2						=	Message.PowerObjectParm

istr_mant.Argumento[1]	=	istr_mant2.Argumento[3]
istr_mant.Argumento[20]	=	istr_mant2.Argumento[20]
istr_mant.Argumento[21]	=	istr_mant2.Argumento[21]
istr_mant.Argumento[22]	=	"1"		//	Pallet de Exportación
istr_mant.Argumento[23]	=	"0"		//	Pallet Mixto
istr_mant.Argumento[24]	=	istr_mant2.Argumento[24]
istr_mant.Argumento[15] =  istr_mant2.Argumento[8] //Lista de Productores de Pallets
istr_mant.argumento[17] =  istr_mant2.argumento[14]//tipo de pantalla; 1 = repalletizado
istr_mant.argumento[24] =  istr_mant2.argumento[15]//cuando es nuevo repalletizaje
istr_mant.argumento[44] =  istr_mant2.Argumento[24]

IF istr_mant.argumento[17] = '1' and istr_mant.argumento[24] <> '1' THEN
	istr_mant.argumento[27] = istr_mant2.argumento[22]
	istr_mant.argumento[28] = istr_mant2.argumento[23]
ELSE
	istr_mant.argumento[27] = ""
	istr_mant.argumento[28] = ""
END IF 
istr_mant.argumento[32] = ""

li_cliente					=	Integer(istr_mant2.Argumento[3])
li_planta					=	Integer(istr_mant2.Argumento[1])
ls_Embalaje					=	EmbalajeCliente(li_Cliente)

dw_2.GetChild("clie_codigo", dw_cliente)
dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("espe_codigo", dw_especie)
dw_2.GetChild("etiq_codigo", dw_etiqueta)

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

idwc_categorias.SetTransObject(sqlca)
idwc_status.SetTransObject(sqlca)
idwc_condicion.SetTransObject(sqlca)
idwc_tratamiento.SetTransObject(sqlca)
idwc_tipofrio.SetTransObject(sqlca)
idwc_destino.SetTransObject(sqlca)

dw_cliente.Retrieve(li_cliente)
dw_planta.Retrieve(1)
dw_especie.Retrieve()
dw_etiqueta.Retrieve()

dw_2.GetChild("tpem_codigo", dw_emba)
dw_emba.SetTransObject(sqlca)
if dw_emba.Retrieve(li_cliente, 'U006') = 0 then
   dw_emba.Insertrow(0)
end if

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

istr_mant.dw				= dw_1
istr_mant.solo_consulta = False

istr_mant.Argumento[1]	=	String(li_cliente)

buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"

//dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)

istr_mant2.dw.ShareData(dw_3)

istr_mant.Argumento[16]=''

pb_nuevo.TriggerEvent(Clicked!)

//GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
//							This.Title, "Acceso a Aplicación", 1)

end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
//
end event

event ue_nuevo;Boolean	lb_TerminaPallet, lb_terminaPucho
Integer	li_Pallets, li_Puchos, li_clien, li_planta, li_especie, li_vari, li_codpak, li_etiq, li_concod
Long		ll_modif1, ll_modif2, ll_prod, ll_cajas, ll_fila, ll_secuen, ll_paen, ll_dataw3
String   ls_emba, ls_calibr
Date     ld_fecemb

ib_ok	= True

CHOOSE CASE  wf_modifica()
	CASE -1
		ib_ok = False
	CASE 0
		ll_modif1	=	dw_1.GetNextModified(0, Primary!)
		ll_modif2	=	dw_2.GetNextModified(0, Primary!)
	
//		IF ib_ModEncab OR dw_1.GetNextModified(0, Primary!) > 0 THEN
		IF dw_1.RowCount() > 0 THEN
			CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
				CASE 1
					Message.DoubleParm = 0
					This.TriggerEvent("ue_guardar")
					IF message.DoubleParm = -1 THEN ib_ok = False
				CASE 3
					ib_ok	= False
					RETURN
			END CHOOSE
		END IF
END CHOOSE

IF ib_ok = False THEN RETURN

dw_1.Reset()
dw_4.Reset()

//pb_eli_det.Enabled		= False
//pb_ins_det.Enabled		= False
//pb_grabar.Enabled			= False
pb_eliminar.Enabled		= False
pb_imprimir.Enabled		= False
istr_mant.solo_consulta	= False
dw_2.Enabled				= True

dw_2.SetRedraw(False)
dw_2.Reset()
dw_2.InsertRow(0)

dw_2.Object.clie_codigo.Protect	=	1
dw_2.Object.plde_codigo.Protect	=	1
dw_2.Object.paen_numero.Protect	=	1
dw_2.Object.paen_fecemb.Protect	=	1
dw_2.Object.paen_cosecha.Protect	=	1
dw_2.Object.espe_codigo.Protect	=	1
dw_2.Object.vari_codigo.Protect	=	1
dw_2.Object.emba_codigo.Protect	=	1
dw_2.Object.paen_concal.Protect	=	1
dw_2.Object.cate_codigo.Protect	=	1
dw_2.Object.trat_codigo.Protect	=	1
dw_2.Object.etiq_codigo.Protect	=	1
dw_2.Object.frio_codigo.Protect	=	1
dw_2.Object.stat_codigo.Protect	=	1
dw_2.Object.tpem_codigo.Protect	=	1
dw_2.Object.cond_codigo.Protect	=	1
dw_2.Object.paen_ccajas.Protect	=	1
dw_2.Object.dest_codigo.Protect	=	1
dw_2.Object.paen_nrasda.Protect	=	1
dw_2.Object.copa_codigo.Protect	=	1
dw_2.Object.clie_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.plde_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.paen_numero.BackGround.Color = RGB(166,180,210)
dw_2.Object.paen_fecemb.BackGround.Color = RGB(166,180,210)
dw_2.Object.paen_cosecha.BackGround.Color= RGB(166,180,210)
dw_2.Object.espe_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.vari_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.emba_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.paen_concal.BackGround.Color = RGB(166,180,210)
dw_2.Object.cate_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.trat_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.etiq_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.frio_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.stat_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.tpem_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.cond_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.paen_ccajas.BackGround.Color = RGB(166,180,210)
dw_2.Object.dest_codigo.BackGround.Color = RGB(166,180,210)
dw_2.Object.paen_nrasda.BackGround.Color = RGB(166,180,210)
dw_2.Object.copa_codigo.BackGround.Color = RGB(166,180,210)

dw_2.SetRedraw(True)
dw_2.SetColumn("paen_palneo")
dw_2.SetFocus()

dw_2.Setitem(1,"clie_codigo", Integer(istr_mant.argumento[1]))
dw_2.SetItem(1,"plde_codigo", Integer(istr_mant2.argumento[1]))
dw_2.SetItem(1,"espe_codigo", gi_CodEspecie)

istr_mant.argumento[3]	=	String(gi_CodEspecie)
istr_mant.argumento[6]	= 	istr_mant2.argumento[1]
istr_mant.argumento[9]	= 	"1"
istr_mant.argumento[10]	=	"0"



end event

event ue_seleccion;//
end event

event ue_modifica_detalle;IF istr_mant2.argumento[7] <> '1' THEN
	Cuentacajas()
	
	IF dw_1.RowCount() > 0 THEN
		istr_mant.argumento[1]	=	String(dw_2.getitemNumber(1,"clie_codigo"))
		istr_mant.argumento[2]	=	String(dw_2.getitemNumber(1,"paen_numero"))
		istr_mant.argumento[3]	=	String(dw_2.getitemNumber(1,"espe_codigo"))
//		istr_mant.argumento[4]	=	String(dw_2.getitemNumber(1,"vari_codigo"))
		
//		istr_mant.argumento[4]	=	String(dw_1.Object.vari_codigo[il_fila]) 
//	   istr_mant.argumento[7]	=	dw_1.Object.emba_codigo[il_fila] 
		
		istr_mant.agrega 			= False
		istr_mant.solo_consulta	= False

		OpenWithParm(iw_mantencion, istr_mant)
	END IF
END IF
end event

event ue_recuperadatos;IF dw_1.RowCount() > 0 THEN
	dw_1.Enabled				=	False
	pb_eli_det.Enabled		=	False
	pb_ins_det.Enabled		=	False
ELSE
	dw_1.Enabled				=	True
	pb_ins_det.Enabled		=	True
END IF
end event

event ue_guardar;//IF not isnull(i_vec_ubicacion[1]) THEN
//	elimina_recprefriopda(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],&
//	                      dw_2.Object.paen_numero[1] );
//END IF

IF dw_1.AcceptText() = -1 THEN
	Message.DoubleParm	=	-1 
	RETURN
END IF

SetPointer(HourGlass!)

Message.DoubleParm	=	0

TriggerEvent("ue_antesguardar")

SetPointer(Arrow!)

IF Message.DoubleParm = -1 THEN
	RETURN
ELSE
	
	IF wf_actualiza_db(False) THEN		
		SetPointer(Arrow!)
	ELSE
		Message.DoubleParm	=	-1	
		SetPointer(Arrow!)
		
		RETURN
	END IF	

	w_maed_recfruprocee_granel.TriggerEvent("ue_guardar")
END IF

TriggerEvent("ue_nuevo")
end event

event ue_nuevo_detalle;date fecha
fecha=(dw_2.Object.paen_fecemb[1])

IF istr_mant2.Argumento[5] = 'dw_mues_repalletdeta' THEN
	istr_mant.borra	= False
	istr_mant.agrega	= True
	cuentacajas()
	istr_mant.argumento[28]=String(dw_2.object.paen_fecemb[1])
	
	istr_mant.argumento[45]=	istr_mant2.argumento[32]		// Fecha de Ingreso
	istr_mant.argumento[46]=	istr_mant2.argumento[33]		// Fecha de Embalaje
	istr_mant.argumento[47]=	istr_mant2.argumento[8]		// Productores
	// w_mant_deta_palletfruta
	
	OpenWithParm(w_mant_deta_palletfruta_repa, istr_mant)
	
	IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
	
	IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
		pb_eliminar.Enabled	= TRUE
		pb_grabar.Enabled		= TRUE
	END IF
	
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)
ELSEIF istr_mant2.Argumento[40] = '1'  THEN	
	istr_mant.borra	= False
	istr_mant.agrega	= True
	cuentacajas()
	istr_mant.argumento[28]=String(dw_2.object.paen_fecemb[1])
	
	OpenWithParm(w_mant_deta_palletfruta_interfecemb, istr_mant)
	
	IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
	
	IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
		pb_eliminar.Enabled	= TRUE
		pb_grabar.Enabled		= TRUE
	END IF
	
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)
ELSE
	call super::ue_nuevo_detalle
END IF
end event

event ue_listo;call super::ue_listo;pb_ins_det.Enabled	= False

dw_2.SetColumn("paen_fecemb")
end event

type dw_1 from w_maed_palletencab`dw_1 within w_maed_palletencab_recepcion_granel
integer x = 0
integer y = 1068
integer width = 2981
integer height = 808
integer taborder = 110
string dataobject = "dw_mues_palletfruta_granel"
boolean righttoleft = false
end type

type dw_2 from w_maed_palletencab`dw_2 within w_maed_palletencab_recepcion_granel
integer x = 37
integer y = 32
string dataobject = "dw_mant_palletencab_granel"
end type

event dw_2::itemchanged;Integer	li_Cliente
Long		ll_null, ll_NroPallet
String	ls_columna, ls_asda, ls_Nulo

SetNull(ll_Null)
SetNull(ls_Nulo)

ls_columna = GetColumnName()

CHOOSE CASE ls_columna

//	CASE "paen_inspec"
//		IF Integer(data) = 1 THEN
//			OpenWithParm(w_proc_inspeccion_informada, istr_mant3)
//			istr_mant3 = Message.PowerObjectParm
//		END IF

	CASE "paen_palneo"
		IF Len(Data) < 5  THEN
			MessageBox("Error", "Ha leído un Código~rde Barra Inválido.", &
							StopSign!)
			This.SetItem(il_Fila, ls_Columna, ls_Nulo)
			RETURN 1
			
		END IF
		
		IF Len(data) > 8 THEN
	//		li_Cliente	=	f_buscacodigocliente(Mid(Data, 1, Len(Data) - 7))
			li_Cliente	=	Integer(Mid(Data, 1, Len(Data) - 7))
			ll_NroPallet=	Long(Mid(Data, Len(Data) - 6))

		ELSE
			li_Cliente											=	0
			ll_NroPallet										=	Long(Data)
		END IF
		
		IF li_Cliente <> Integer(istr_mant.argumento[1]) THEN
			MessageBox("Error", "Ha leído un Código~rde Barra Inválido.", &
							StopSign!)
			This.SetItem(il_Fila, ls_Columna, ls_Nulo)
			RETURN 1
		END IF
						
		IF ( Duplicado(Integer(istr_mant.argumento[1]),ll_NroPallet) OR &
		   Not ExisteCapturaPallet(Integer(istr_mant.argumento[1]),&
				Integer(istr_mant.argumento[6]), ll_NroPallet) ) THEN
			This.SetItem(il_Fila, ls_Columna, ls_Nulo)
			RETURN 1
		END IF

		IF Not cargapallet(Integer(istr_mant.argumento[1]),&
			Integer(istr_mant.Argumento[6]),ll_NroPallet) THEN
		   Messagebox("Cuidado", "Número de Pallet No Existe!!!")
			This.SetItem(il_Fila, ls_Columna, ls_Nulo)
			RETURN 1			
		END IF
		This.SetItem(il_Fila,"paen_numero",ll_NroPallet)

	CASE "tpem_codigo"
		IF NOT comparacajas(data) THEN
//			dw_1.Object.paen_ccajas[il_Fila] = ii_tpemccajas
			MessageBox("Error", "Total de Cajas No Coincide con Tipo.", &
							StopSign!)
			This.SetItem(il_Fila, ls_Columna, ls_Nulo)
			RETURN 1
		END IF

//	CASE "tpem_codigo"
//		IF dw_2.object.paen_tipopa[Row] = 1 THEN
//			IF ExisteTipoEmbalaje(data) = False THEN
//				dw_2.SetItem(1, "emba_codigo", ll_null)
//				RETURN 1
//			END IF
//		END IF
		
END CHOOSE

//call super::itemchanged

//CHOOSE CASE ls_columna
//
//	CASE "vari_codigo"
//		IF istr_mant.argumento[17] = '1'and istr_mant.argumento[24] <> '1' THEN
//			IF Pos(istr_mant.argumento[27], "," + Data) = 0 THEN
//				messagebox("Atención","Variedad no corresponde a Pallets originales", Exclamation!, Ok!)
//				dw_2.SetItem(row, "vari_codigo", ll_null)
//				RETURN 1
//			END IF
//		END IF
//		
//	CASE "paen_fecemb"
//		istr_mant.Argumento[40] = Data
//
//END CHOOSE
//

IF ii_yaexiste = 1 THEN
	ii_yaexiste = 0
	dw_2.SetItem(1, "paen_numero", ll_null)
	RETURN 1
END IF
end event

type pb_nuevo from w_maed_palletencab`pb_nuevo within w_maed_palletencab_recepcion_granel
integer x = 3163
end type

type pb_eliminar from w_maed_palletencab`pb_eliminar within w_maed_palletencab_recepcion_granel
integer x = 3163
end type

type pb_grabar from w_maed_palletencab`pb_grabar within w_maed_palletencab_recepcion_granel
integer x = 3154
integer y = 680
boolean enabled = true
end type

event pb_grabar::clicked;
//dw_6.Reset()
//wf_FechaEmbalaje()

call super:: clicked


end event

type pb_imprimir from w_maed_palletencab`pb_imprimir within w_maed_palletencab_recepcion_granel
integer x = 3163
integer y = 900
end type

type pb_salir from w_maed_palletencab`pb_salir within w_maed_palletencab_recepcion_granel
integer x = 3163
end type

event pb_salir::clicked;Integer	li_respue

IF istr_mant2.agrega = False AND istr_mant2.borra = False THEN
	CloseWithReturn(Parent, istr_mant2)
ELSE
	IF dw_3.RowCount() < ii_CantPallets + ii_CantPuchos THEN
		li_respue	=	MessageBox("ADVERTENCIA","No se ha completado la cantidad de Pallets~r" + &
							"indicados en la Recepción.~r~rDesea retornar a ventana de recepción.", &
							Question!, YesNo!)
			IF li_respue = 1 THEN
				CloseWithReturn(Parent, istr_mant2)
			ELSE
				pb_nuevo.SetFocus()
			END IF
	ELSE
		CloseWithReturn(Parent, istr_mant2)
	END IF
END IF
	

end event

type pb_ins_det from w_maed_palletencab`pb_ins_det within w_maed_palletencab_recepcion_granel
boolean visible = false
integer x = 3163
integer y = 1480
end type

type pb_eli_det from w_maed_palletencab`pb_eli_det within w_maed_palletencab_recepcion_granel
boolean visible = false
integer x = 3163
end type

type pb_buscar from w_maed_palletencab`pb_buscar within w_maed_palletencab_recepcion_granel
boolean visible = false
integer x = 3163
end type

type dw_histoencab from w_maed_palletencab`dw_histoencab within w_maed_palletencab_recepcion_granel
end type

type dw_histofruta from w_maed_palletencab`dw_histofruta within w_maed_palletencab_recepcion_granel
end type

type dw_4 from datawindow within w_maed_palletencab_recepcion_granel
boolean visible = false
integer y = 2072
integer width = 3159
integer height = 232
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_palletencab"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_maed_palletencab_recepcion_granel
boolean visible = false
integer y = 1316
integer width = 3378
integer height = 652
integer taborder = 100
boolean bringtotop = true
boolean titlebar = true
string dataobject = "dw_mues_recfruproced_granel"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
end type

type dw_5 from datawindow within w_maed_palletencab_recepcion_granel
boolean visible = false
integer x = 1838
integer y = 1808
integer width = 686
integer height = 400
integer taborder = 110
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_spro_palletfruta"
boolean livescroll = true
end type

