$PBExportHeader$w_maed_palletencab_recepcion_trans_inter.srw
forward
global type w_maed_palletencab_recepcion_trans_inter from w_maed_palletencab_trans
end type
type dw_3 from datawindow within w_maed_palletencab_recepcion_trans_inter
end type
type dw_4 from datawindow within w_maed_palletencab_recepcion_trans_inter
end type
type dw_6 from datawindow within w_maed_palletencab_recepcion_trans_inter
end type
type dw_5 from datawindow within w_maed_palletencab_recepcion_trans_inter
end type
type dw_7 from datawindow within w_maed_palletencab_recepcion_trans_inter
end type
end forward

global type w_maed_palletencab_recepcion_trans_inter from w_maed_palletencab_trans
integer height = 2196
dw_3 dw_3
dw_4 dw_4
dw_6 dw_6
dw_5 dw_5
dw_7 dw_7
end type
global w_maed_palletencab_recepcion_trans_inter w_maed_palletencab_recepcion_trans_inter

type variables
Str_mant	istr_mant2

DataWindowChild	idwc_categorias, idwc_status, idwc_condicion, &
						idwc_tratamiento, idwc_tipofrio, idwc_destino
										  
Boolean		lb_MensPallet=False, lb_MensPucho=False
Integer		ii_CantPallets, ii_CantPuchos;

/*================================================================
Vector que guarda datos sobre la ubicacion de un pallet, el vector 
se compondrá por la sgte asignacion:
i_vec_ubicacion[1]=camara, i_vec_ubicacion[2]=calle, 
i_vec_ubicacion[3]=base,   i_vec_ubicacion[4]=posicion,
i_vec_ubicacion[5]=tipo camara
================================================================*/
integer 		i_vec_ubicacion[5], ii_existe
Boolean	lb_TerminaPallet, lb_terminaPucho


end variables

forward prototypes
public function boolean duplicadoinspeccion (long nroinspeccion)
public function boolean nroinspeccion (long nroinspeccion)
public function boolean duplicadoinspecdet (long nroinspeccion)
public subroutine elimina_recprefriopda (integer li_clie_codigo, integer li_plde_codigo, long ll_paen_numero)
public function boolean existe_recprefriopda (integer li_clie_codigo, integer li_plde_codigo, long ll_paen_numero)
public subroutine existeotraplanta (long al_pallet)
public function boolean existepallet (string ls_columna)
public function boolean grabadocrel ()
public function boolean noesnuevo (integer planta, integer cliente, long numero)
public function boolean noexistetarja (integer planta, integer folio, integer cliente, long pallet)
public function boolean noexistetarjare (integer planta, integer folio, integer cliente, long pallet)
public function integer palletsinencab (integer planta, integer cliente, long numero)
protected function boolean wf_actualiza_db (boolean borrando)
public function boolean duplicado (string campo)
public function boolean existepallet_definitivo (string ls_columna)
public subroutine inserta_devolucion ()
public subroutine actualiza_detalle_trans ()
public function boolean existe_pallet_devolucion (string ls_columna)
public function boolean existepallet_repa (string ls_columna)
public function boolean borra_detalle (long al_pallet)
public subroutine habilitaingreso ()
public function boolean existe_palletdespachado (long al_pallet)
end prototypes

public function boolean duplicadoinspeccion (long nroinspeccion);Long		ll_fila
Integer  li_planta,li_cliente,li_tipoin,li_secuen
Date		ld_fechai

SetNull(li_secuen)

li_tipoin	=	1
li_planta	= dw_2.Object.plde_codigo[1]
li_cliente	= dw_2.Object.clie_codigo[1]

ll_fila	= dw_5.Find("inpe_tipoin = " + String(li_tipoin) + " AND inpe_numero = " + String(nroinspeccion) + &
               " AND plde_codigo = " + String(li_planta) + " AND clie_codigo = " + String(li_cliente), &
					1, dw_5.RowCount())

IF ll_fila > 0 THEN
	RETURN True
ELSE

	SELECT inpe_secuen INTO :li_secuen
	FROM dba.INSPECPALENC_trans
	WHERE inpe_tipoin = :li_tipoin
	AND   inpe_numero = :nroinspeccion
	AND   clie_codigo = :li_cliente
	AND   plde_codigo = :li_planta;
	
	IF sqlca.SQLCode = -1 THEN
		RETURN True
	ELSEIF Isnull(li_secuen) THEN
		    RETURN False
		ELSE
			 RETURN True
		END IF
   END IF

end function

public function boolean nroinspeccion (long nroinspeccion);Long		ll_fila
Integer  li_planta,li_cliente,li_tipoin,li_secuen
Date		ld_fechai

SetNull(li_secuen)

li_tipoin	=	1
li_planta	= dw_2.Object.plde_codigo[1]
li_cliente	= dw_2.Object.clie_codigo[1]

ll_fila	= dw_5.Find("inpe_tipoin = " + String(li_tipoin) + " AND inpe_numero = " + String(nroinspeccion) + &
               " AND plde_codigo = " + String(li_planta) + " AND clie_codigo = " + String(li_cliente), &
					1, dw_5.RowCount())

IF ll_fila > 0 THEN
	RETURN True
ELSE

	SELECT inpe_secuen INTO :li_secuen
	FROM dba.INSPECPALENC
	WHERE inpe_tipoin = :li_tipoin
	AND   inpe_numero = :nroinspeccion
	AND   clie_codigo = :li_cliente
	AND   plde_codigo = :li_planta;
	
	IF sqlca.SQLCode = -1 THEN
		RETURN True
	ELSEIF Isnull(li_secuen) THEN
		    RETURN False
		ELSE
			 RETURN True
		END IF
   END IF

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
	FROM dba.INSPECPALDET_trans
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

public subroutine elimina_recprefriopda (integer li_clie_codigo, integer li_plde_codigo, long ll_paen_numero);//
DELETE FROM dba.recprefriopda WHERE
	clie_codigo= :li_clie_codigo AND 
	plde_codigo= :li_plde_codigo AND
	paen_numero= :ll_paen_numero;
	
	/*control SQl*/


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
	DBA.PALLETENCAB_trans WHERE
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

public subroutine existeotraplanta (long al_pallet);Integer	li_Cliente, li_Planta, li_Estado

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

public function boolean existepallet (string ls_columna);Integer		li_cliente, li_tiporec, li_ptadest, li_ptaorig, li_cantid, li_sinencabezado,&
            li_donde, li_usda
Long			ll_nropal, ll_fila, ll_filad, ll_pcopda
String		ls_embarque, ls_embalaje
Boolean		lb_retorno = False
DataStore	ds_encabezado, ds_detalle

ds_encabezado	 =	CREATE DataStore 
ds_detalle		 =	CREATE DataStore

li_cliente					= 	dw_2.Object.clie_codigo[1]
li_ptadest					=	dw_2.Object.plde_codigo[1]
li_tiporec					=	Integer(istr_mant.argumento[20])
li_ptaorig					=	Integer(istr_mant.argumento[21])
ls_embarque					=	istr_mant.argumento[44]
ll_nropal 					= 	Long(ls_columna)
istr_mant.argumento[2]	= 	String(ll_nropal)
ls_Embalaje					=	EmbalajeCliente(li_Cliente)

li_sinencabezado			=	PalletSinEncab(li_ptadest,li_Cliente,ll_nropal)

SELECT	Count(*)
	INTO	:li_cantid
	FROM	dba.palletencab_trans
	WHERE	clie_codigo		= :li_cliente
	AND	paen_numero    = :ll_nropal ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
	lb_retorno = True
ELSEIF li_cantid > 0 THEN
	CHOOSE CASE li_tiporec
		CASE 0,1,4,5
			
			   SELECT	Count(*)
				INTO	:li_usda
				FROM	dba.palletencab_trans
				WHERE	clie_codigo		= :li_cliente
				AND	paen_numero    = :ll_nropal
				AND   paen_estado in (7,8);
			
////			IF li_sinencabezado > 0 THEN
//				MessageBox("Atención","Nro. de Pallet ya existe para este Cliente.  Ingrese otro.", &
//					Exclamation!, Ok!)
//				lb_retorno = True
//			
////			END IF	

            IF li_sinencabezado = 0 AND li_usda = 0 THEN
               
					
					super::existepallet(ls_columna)
					
					
            ELSE
	   			MessageBox("Atención","Nro. de Pallet ya existe para este Cliente.  Ingrese otro.", &
					Exclamation!, Ok!)
		   		lb_retorno = True
			
   			END IF	
					
			
		CASE 2
			SELECT	Count(*)
				INTO 	:li_cantid
				FROM	dba.palletencab_trans
				WHERE	clie_codigo	=	:li_cliente
				AND	paen_numero	=	:ll_nropal
				AND	plde_codigo	=	:li_ptaorig
				AND   paen_estado not in (7,8);

				IF sqlca.SQLCode = -1 THEN
					lb_retorno = True
				ELSEIF li_cantid = 0 THEN
						SELECT	Count(*)
						INTO 	:li_donde
						FROM	dba.palletencab_trans
						WHERE	clie_codigo	=	:li_cliente
						AND	paen_numero	=	:ll_nropal
						AND	plde_codigo	=	:li_ptadest ;
					   IF sqlca.SQLCode = -1 THEN
					      RETURN True
						ELSEIF li_donde > 0 THEN
								 MessageBox("Atención","Pallet ya existe en esta Planta.", &
								 Exclamation!, Ok!)
								 RETURN True
						END IF
					END IF			
			IF sqlca.SQLCode = -1 THEN
				F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
				lb_retorno = True
			ELSEIF li_cantid > 0 THEN
				ds_encabezado.DataObject	=	"dw_mant_palletencab"
				ds_detalle.DataObject		=	"dw_mues_palletfruta"
				
				ds_encabezado.GetChild("clie_codigo", dw_cliente)
				ds_encabezado.GetChild("plde_codigo", dw_planta)
				ds_encabezado.GetChild("espe_codigo", dw_especie)
				ds_encabezado.GetChild("etiq_codigo", dw_etiqueta)
				ds_encabezado.GetChild("tpem_codigo", dw_emba)
				
				dw_cliente.SetTransObject(sqlca)
				dw_planta.SetTransObject(sqlca)
				dw_especie.SetTransObject(sqlca)
				dw_etiqueta.SetTransObject(sqlca)
				dw_emba.SetTransObject(sqlca)
				
				dw_cliente.Retrieve(li_cliente)
				dw_planta.Retrieve(1)
				dw_especie.Retrieve()
				dw_etiqueta.Retrieve()
				dw_emba.Retrieve(li_cliente, ls_Embalaje)
								
				ds_encabezado.SetTransObject(sqlca)
				ds_detalle.SetTransObject(sqlca)

				ds_encabezado.Retrieve(li_cliente, ll_nropal, li_ptaorig)
				ds_detalle.Retrieve(li_cliente, ll_nropal, li_ptaorig)
				//
				dw_2.SetItem(1, "paen_tipopa", ds_encabezado.Object.paen_tipopa[1])
				dw_2.SetItem(1, "tpem_codigo", ds_encabezado.Object.tpem_codigo[1])
				dw_2.SetItem(1, "espe_codigo", ds_encabezado.Object.espe_codigo[1])
				dw_2.SetItem(1, "vari_codigo", ds_encabezado.Object.vari_codigo[1])
				dw_2.SetItem(1, "vari_nombre", ds_encabezado.Object.vari_nombre[1])
				dw_2.SetItem(1, "tiem_codigo", ds_encabezado.Object.tiem_codigo[1])
				dw_2.SetItem(1, "emba_codigo", ds_encabezado.Object.emba_codigo[1])
				dw_2.SetItem(1, "emba_nombre", ds_encabezado.Object.emba_nombre[1])
				dw_2.SetItem(1, "cate_codigo", ds_encabezado.Object.cate_codigo[1])
				dw_2.SetItem(1, "etiq_codigo", ds_encabezado.Object.etiq_codigo[1])
				dw_2.SetItem(1, "stat_codigo", ds_encabezado.Object.stat_codigo[1])
				dw_2.SetItem(1, "trat_codigo", ds_encabezado.Object.trat_codigo[1])
				dw_2.SetItem(1, "frio_codigo", ds_encabezado.Object.frio_codigo[1])
				dw_2.SetItem(1, "cond_codigo", ds_encabezado.Object.cond_codigo[1])
				dw_2.SetItem(1, "dest_codigo", ds_encabezado.Object.dest_codigo[1])
				dw_2.SetItem(1, "paen_fecemb", ds_encabezado.Object.paen_fecemb[1])
				dw_2.SetItem(1, "paen_cosecha", ds_encabezado.Object.paen_cosecha[1])
				dw_2.SetItem(1, "paen_altura", ds_encabezado.Object.paen_altura[1])
				dw_2.SetItem(1, "paen_ccajas", ds_encabezado.Object.paen_ccajas[1])
				dw_2.SetItem(1, "tmvp_codigo", ds_encabezado.Object.tmvp_codigo[1])
				dw_2.SetItem(1, "paen_fecini", ds_encabezado.Object.paen_fecini[1])
				dw_2.SetItem(1, "paen_horain", ds_encabezado.Object.paen_horain[1])
				dw_2.SetItem(1, "cama_codigo", ds_encabezado.Object.cama_codigo[1])
				dw_2.SetItem(1, "paen_calle", ds_encabezado.Object.paen_calle[1])

				dw_2.SetItem(1, "paen_base", ds_encabezado.Object.paen_base[1])
				dw_2.SetItem(1, "paen_posici", ds_encabezado.Object.paen_posici[1])
				dw_2.SetItem(1, "paen_estado", 1)
				dw_2.SetItem(1, "paen_inspec", ds_encabezado.Object.paen_inspec[1])
				dw_2.SetItem(1, "paen_concal", ds_encabezado.Object.paen_concal[1])
				dw_2.SetItem(1, "paen_pexpor", ds_encabezado.Object.paen_pexpor[1])
				dw_2.SetItem(1, "paen_pmixto", ds_encabezado.Object.paen_pmixto[1])
				dw_2.SetItem(1, "copa_codigo", ds_encabezado.Object.copa_codigo[1])
				//
				FOR ll_fila	=	1 TO ds_detalle.RowCount()
					ll_filad	=	dw_1.InsertRow(0)
					dw_1.SetItem(ll_filad, "clie_codigo", li_cliente)
					dw_1.SetItem(ll_filad, "paen_numero", ll_nropal)
					dw_1.SetItem(ll_filad, "espe_codigo", ds_detalle.Object.espe_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "vari_codigo", ds_detalle.Object.vari_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "emba_codigo", ds_detalle.Object.emba_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "prod_codigo", ds_detalle.Object.prod_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "productores_prod_nombre", ds_detalle.Object.productores_prod_nombre[ll_fila])
					dw_1.SetItem(ll_filad, "cond_codigo", ds_detalle.Object.cond_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "etiq_codigo", ds_detalle.Object.etiq_codigo[ll_fila])
					dw_1.SetItem(ll_filad, "plde_codigo", li_ptadest)
					dw_1.SetItem(ll_filad, "pafr_calibr", ds_detalle.Object.pafr_calibr[ll_fila])
					dw_1.SetItem(ll_filad, "pafr_secuen", ds_detalle.Object.pafr_secuen[ll_fila])
					dw_1.SetItem(ll_filad, "pafr_ccajas", ds_detalle.Object.pafr_ccajas[ll_fila])
					dw_1.SetItem(ll_filad, "pafr_nrlote", ds_detalle.Object.pafr_nrlote[ll_fila])
					
					dw_1.SetItem(ll_Filad, "pafr_copack", ds_detalle.Object.pafr_copack[ll_Fila])
					dw_1.SetItem(ll_Filad, "pafr_fecemb", ds_detalle.Object.pafr_fecemb[ll_Fila])
					dw_1.SetItem(ll_Filad, "pafr_cuart1", ds_detalle.Object.pafr_cuart1[ll_Fila])
					dw_1.SetItem(ll_filad, "pafr_huert1", ds_detalle.Object.pafr_huert1[ll_Fila])
				NEXT
				istr_mant.argumento[3]	=	String(dw_2.Object.espe_codigo[1])
				istr_mant.argumento[4]	=	String(dw_2.Object.vari_codigo[1])
				istr_mant.argumento[5]	= 	dw_2.Object.vari_nombre[1]
				istr_mant.argumento[6]	= 	String(dw_2.Object.plde_codigo[1])
				istr_mant.argumento[7]	= 	dw_2.Object.emba_codigo[1]
				istr_mant.argumento[8]	= 	dw_2.Object.emba_nombre[1]
				istr_mant.argumento[9]	= 	String(dw_2.Object.etiq_codigo[1])
				istr_mant.argumento[10] = 	String(dw_2.Object.cond_codigo[1])
				istr_mant.argumento[12] = 	""
				cuentacajas()
				ExisteTipoEmbalaje(String(dw_2.Object.tpem_codigo[1]))
				ExistePlanta(dw_2.Object.plde_codigo[1])
				Existecondicion(dw_2.Object.cond_codigo[1])
				IF noesnuevo(li_ptadest,li_cliente, ll_nropal) THEN
					pb_eliminar.Enabled	= False
					pb_grabar.Enabled		= False
					UPDATE dba.palletencab	SET
					   paen_estado = 1
						WHERE	clie_codigo	=	:li_cliente
						AND	paen_numero	=	:ll_nropal
						AND	plde_codigo	=	:li_ptadest ;
					commit;
				ELSE
					pb_eliminar.Enabled	= True
					pb_grabar.Enabled		= True
				END IF

				END IF
		CASE 3,7
			SELECT	Count(*)
				INTO 	:li_cantid
				FROM	dba.palletencab_trans
				WHERE	clie_codigo	=	:li_cliente
				AND	paen_numero	=	:ll_nropal
				AND	plde_codigo	=	:li_ptadest ;
							
			IF sqlca.SQLCode = -1 THEN
				F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
				lb_retorno = True
			ELSEIF li_cantid = 0 THEN
				MessageBox("Atención","Nro. de Pallet No Existe en esta Planta.", &
					Exclamation!, Ok!)
				lb_retorno = True
			ELSE
				IF li_tiporec = 3 THEN
					SELECT	Count(*)
						INTO 	:li_cantid
						FROM	dba.despafrigode as det, dba.despafrigoen as enc
						WHERE	det.plde_codigo	=	:li_ptadest
						AND	det.clie_codigo	=	:li_cliente
						AND	det.paen_numero	=	:ll_nropal
						AND	enc.plde_codigo	=	det.plde_codigo
						AND	enc.defe_numero	=	det.defe_numero
						AND	enc.embq_codigo	=	:ls_embarque ;
				ELSE
					SELECT	Count(*)
						INTO 	:li_cantid
						FROM	dba.palletencab_trans
						WHERE	clie_codigo	=	:li_cliente
						AND	paen_numero	=	:ll_nropal
						AND	plde_codigo	=	:li_ptadest
						AND   paen_estado =  8;

				END IF
				IF sqlca.SQLCode = -1 THEN
					F_errorbasedatos(sqlca,"Lectura tablas Despafrigode / Despafrigoen")
					lb_retorno = True
				ELSEIF IsNull(li_cantid) OR li_cantid = 0 THEN
					MessageBox("Atención","Pallet No Disponible.", &
						Exclamation!, Ok!)
					   lb_retorno = True
				ELSE
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
					ExisteTipoEmbalaje(String(dw_2.Object.tpem_codigo[1]))
					ExistePlanta(dw_2.Object.plde_codigo[1])
					Existecondicion(dw_2.Object.cond_codigo[1])
					IF li_tiporec = 3 THEN
						dw_2.SetItem(1, "paen_estado", 1)
					ELSE
						dw_2.SetItem(1, "paen_estado", 7)
					END IF
				END IF
			END IF

		CASE 6
			
			SELECT	Count(*)
				INTO 	:li_cantid
				FROM	dba.palletencab_trans
				WHERE	clie_codigo	=	:li_cliente
				AND	paen_numero	=	:ll_nropal
				AND	plde_codigo	=	:li_ptadest 				
				AND   paen_estado not in (7,8);
							
			IF sqlca.SQLCode = -1 THEN
				F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
				lb_retorno = True
			ELSEIF li_cantid = 0 THEN
				MessageBox("Atención","Pallet NO ha pertenecido a esta Planta.", &
					Exclamation!, Ok!)
			//	lb_retorno = True
			ELSE
							
				SELECT	Count(*)
					INTO 	:li_cantid
					FROM	dba.palletencab
					WHERE	clie_codigo	=	:li_cliente
					AND	paen_numero	=	:ll_nropal
					AND	plde_codigo	=	:li_ptadest
				   AND   paen_estado =  1;
					
					IF li_cantid > 0 THEN
						MessageBox("Atención","Pallet con estado de Existencia en esta Planta.", &
						Exclamation!, Ok!)
						//lb_retorno = True
				   ELSE					
		
						SELECT	Count(*)
							INTO 	:li_cantid
							FROM	dba.despafrigode as det, dba.despafrigoen as enc
							WHERE	det.plde_codigo	=	:li_ptadest
							AND	det.clie_codigo	=	:li_cliente
							AND	det.paen_numero	=	:ll_nropal
							AND	enc.plde_codigo	=	det.plde_codigo
							AND	enc.defe_numero	=	det.defe_numero;
						
						IF sqlca.SQLCode = -1 THEN
							F_errorbasedatos(sqlca,"Lectura tablas Despafrigode / Despafrigoen")
							lb_retorno = True
						ELSEIF li_cantid = 0 THEN
							MessageBox("Atención","No Existe Despacho para Reingresar Nro.de Pallet.", &
								Exclamation!, Ok!)
							lb_retorno = True
						ELSE
						//	istr_mant.argumento[6] = String(li_ptaorig)
							
							
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
							ExisteTipoEmbalaje(String(dw_2.Object.tpem_codigo[1]))
							ExistePlanta(dw_2.Object.plde_codigo[1])
							Existecondicion(dw_2.Object.cond_codigo[1])
							dw_2.SetItem(1, "paen_estado", 1)
						//	dw_2.Object.plde_codigo[1] = li_ptadest
//							dw_2.SetItem(1, "paen_inspec", 0)
//							dw_2.SetItem(1, "dest_codigo", 999)

							istr_mant.Solo_Consulta = False
						END IF
					END IF
				END IF					
					
	END CHOOSE
ELSEIF li_tiporec = 7 THEN
		MessageBox("Atención","No Existe Pallet en Sitio USDA.  Ingrese otro.", &
		Exclamation!, Ok!)
  		lb_retorno = True
ELSE
	SELECT Count(*)
		INTO	:ll_pcopda
		FROM	dba.spro_cajasprodpallet
		WHERE	clie_codigo	= 	:li_cliente
		AND	capr_numpal = 	:ll_nropal
		AND	plde_codigo	=	:li_ptadest;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla spro_cajasprodpallet")
		lb_retorno = True
	ELSEIF ll_pcopda > 0 THEN	
			MessageBox("Atención","El Numero de Pallet Ingresado es un Ingreso Caja a Caja. Ingrese Otro.", Exclamation!, Ok!)
			lb_retorno = True	
	END IF
END IF

DESTROY ds_encabezado
DESTROY ds_detalle

RETURN lb_retorno
end function

public function boolean grabadocrel ();String	ls_embala
Integer	li_cliente, li_cancaj, li_planta
Long		ll_pallet, ll_docrel

li_cliente	= Integer(istr_mant.argumento[1])
li_planta	= Integer(istr_mant.argumento[6])
ll_pallet   = Long(istr_mant.argumento[2])

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
	
	UPDATE dba.palletfruta_trans SET
		pafr_docrel = :ll_docrel
		WHERE clie_codigo = :li_cliente
		AND	plde_codigo = :li_planta
		AND	paen_numero = :ll_pallet;
		
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

public function boolean noesnuevo (integer planta, integer cliente, long numero);Long	registros
  
SELECT	Count(*)  
	INTO	:registros
  	FROM	dba.palletencab_trans
  	WHERE plde_codigo	=	:planta
	AND  	clie_codigo	=	:cliente
	AND 	paen_numero	=	:numero ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla palletencab")
	RETURN False
ELSEIF registros > 0 THEN
	RETURN True
END IF

RETURN False
end function

public function boolean noexistetarja (integer planta, integer folio, integer cliente, long pallet);Long	registros
  
SELECT	Count(*)  
	INTO	:registros
  	FROM	dba.recfruproced_trans
  	WHERE plde_codigo	=	:planta
  	AND	rfpe_numero	=	:folio
	AND  	clie_codigo	=	:cliente
	AND 	paen_numero	=	:pallet ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Recfruproced")
	RETURN FALSE
ELSEIF registros > 0 THEN
	MessageBox("Atención","Nro de Pallet ya ingresado para el Nro. de Folio", Exclamation!, Ok!)
	RETURN FALSE
END IF

RETURN TRUE
end function

public function boolean noexistetarjare (integer planta, integer folio, integer cliente, long pallet);Long	registros
  
  SELECT Count(paen_numero)  
    INTO :registros
    FROM dba.Repalletdeta
   WHERE plde_codigo = :planta AND  
         repe_numero = :folio AND  
         clie_codigo = :cliente AND  
         paen_numero = :pallet;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Repalletdeta")
	RETURN FALSE
ELSEIF registros > 0 THEN
	MessageBox("Atención","Nro de Pallet ya ingresado para el Nro. de Folio", Exclamation!, Ok!)
	RETURN FALSE
END IF

RETURN TRUE
end function

public function integer palletsinencab (integer planta, integer cliente, long numero);Long	registros=0,registros1=0, registros2=0
  
SELECT	Count(*)  
	INTO	:registros1
  	FROM	dba.recfruproced_trans as re, dba.palletencab_trans as pe
  	WHERE re.plde_codigo	=	:planta
	AND  	re.clie_codigo	=	:cliente
	AND 	re.paen_numero	=	:numero
	AND   pe.paen_estado =  1
	AND   re.clie_codigo =  pe.clie_codigo
	AND   re.plde_codigo =  pe.plde_codigo
	AND   re.paen_numero =  pe.paen_numero;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Recfruproced")
	registros1=1
END IF

SELECT	Count(*)  
	INTO	:registros2
  	FROM	dba.repalletdeta
  	WHERE plde_codigo	=	:planta
	AND  	clie_codigo	=	:cliente
	AND 	paen_numero	=	:numero ;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Repalletdeta")
	registros2=1
END IF

registros = registros1 + registros2

RETURN registros
end function

protected function boolean wf_actualiza_db (boolean borrando);if not dw_2.uf_check_required(0) then return false
if not dw_1.uf_validate(0) then return false
if borrando then
	if dw_1.Update() = 1 and dw_2.Update() = 1 then
		commit;
		if sqlca.sqlcode <> 0 then
				f_errorbasedatos(sqlca,"Lista la tabla")
			return false
		else
			return true
		end if 
	else
		rollback;
		if sqlca.sqlcode <> 0 then f_errorbasedatos(sqlca,"Lista la tabla")
		return false
	end if
else
	if dw_2.update() = 1 and dw_1.Update() = 1  and dw_5.Update() = 1 and dw_4.Update() = 1 then 
		commit;
		if sqlca.sqlcode <> 0 then
			f_errorbasedatos(sqlca,"Lista la tabla")
			return false
		else
			return true
		end if 
	else
		rollback;
		if sqlca.sqlcode <> 0 then f_errorbasedatos(sqlca,"Lista la tabla")
		return false
	end if
end if

return true
end function

public function boolean duplicado (string campo);Long		ll_fila
String	ls_codigo,ls_cliente,ls_numero

//ls_codigo	= String(dw_2.Object.plde_codigo[1])
ls_codigo	=	istr_mant2.Argumento[1]
ls_numero	= 	istr_mant2.Argumento[2]
ls_cliente	= 	String(dw_2.Object.clie_codigo[1])

IF istr_mant2.Argumento[5] = 'dw_mues_repalletdeta' THEN
	ll_fila	= dw_3.Find("plde_codigo = " + ls_codigo + " AND clie_codigo = " + ls_cliente + " AND paen_numero =	" + campo, 1, dw_3.RowCount())
ELSE
	ll_fila	= dw_3.Find("plde_codigo = " + ls_codigo + " AND clie_codigo = " + ls_cliente + " AND paen_numero =	" + campo, 1, dw_3.RowCount())
END IF

IF ll_fila > 0 THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean existepallet_definitivo (string ls_columna);Integer	li_cliente, li_planta, li_Movto
Long		ll_nropal, ll_pcopda, ll_cantid, ll_PFruta
Boolean	lb_retorno = False

li_cliente					=	Integer(dw_2.GetItemNumber(1, "clie_codigo"))
li_planta					=	Integer(dw_2.GetItemNumber(1, "plde_codigo"))
ll_nropal 					=	Long(ls_columna)

SELECT	Count(*)
	INTO	:ll_cantid
	FROM	dba.palletencab
	WHERE	clie_codigo	= 	:li_cliente
	AND	paen_numero = 	:ll_nropal
	AND	plde_codigo	=	:li_planta
	AND   paen_estado = 1;
	
SELECT	Count(*)
	INTO	:ii_existe
	FROM	dba.palletencab
	WHERE	clie_codigo	= 	:li_cliente
	AND	paen_numero = 	:ll_nropal
	AND	plde_codigo	=	:li_planta;	
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
	lb_retorno = True
ELSEIF ll_cantid > 0 THEN
	MessageBox("Atención","El Numero de Pallet Ingresado es un Ingreso Definitivo y está en Existencia. Ingrese Otro.", Exclamation!, Ok!)
	istr_mant.UsuarioSoloConsulta	=	True
	lb_retorno = True		
END IF	
	   
RETURN lb_retorno

end function

public subroutine inserta_devolucion ();Long	ll_fila, ll_new

IF dw_6.RowCount() > 0 THEN
	FOR ll_fila = 1 TO dw_6.RowCount()
		
		ll_new 	= 	dw_1.InsertRow(0)
		
		dw_1.Object.clie_codigo[ll_new]	=  dw_6.Object.clie_codigo[ll_fila]   
		dw_1.Object.paen_numero[ll_new] 	=	dw_6.Object.paen_numero[ll_fila] 
		dw_1.Object.espe_codigo[ll_new] 	=	dw_6.Object.espe_codigo[ll_fila] 
		dw_1.Object.vari_codigo[ll_new]  = 	dw_6.Object.vari_codigo[ll_fila]
		dw_1.Object.emba_codigo[ll_new]  =	dw_6.Object.emba_codigo[ll_fila]
		dw_1.Object.prod_codigo[ll_new]  =	dw_6.Object.prod_codigo[ll_fila]  
		dw_1.Object.cond_codigo[ll_new]  =	dw_6.Object.cond_codigo[ll_fila]  
		dw_1.Object.etiq_codigo[ll_new]  =	dw_6.Object.etiq_codigo[ll_fila]  
		dw_1.Object.plde_codigo[ll_new]  =	dw_6.Object.plde_codigo[ll_fila]  
		dw_1.Object.pafr_calibr[ll_new]  =	dw_6.Object.pafr_calibr[ll_fila]  
		dw_1.Object.pafr_secuen[ll_new]  =	dw_6.Object.pafr_secuen[ll_fila]  
		dw_1.Object.pafr_ccajas[ll_new]  =	dw_6.Object.pafr_ccajas[ll_fila]  
		dw_1.Object.pafr_nrlote[ll_new]  =	dw_6.Object.pafr_nrlote[ll_fila]  
		dw_1.Object.pafr_copack[ll_new]  =	dw_6.Object.pafr_copack[ll_fila]  
		dw_1.Object.pafr_varrot[ll_new]  =	dw_6.Object.pafr_varrot[ll_fila]  
		dw_1.Object.pafr_prdrot[ll_new]  =	dw_6.Object.pafr_prdrot[ll_fila]  
		dw_1.Object.pafr_calrot[ll_new]  =	dw_6.Object.pafr_calrot[ll_fila]  
		dw_1.Object.pafr_huert1[ll_new]  =	dw_6.Object.pafr_huert1[ll_fila]  
		dw_1.Object.pafr_cuart1[ll_new]  =	dw_6.Object.pafr_cuart1[ll_fila]  
		dw_1.Object.pafr_fecemb[ll_new]  =	dw_6.Object.pafr_fecemb[ll_fila]  
		dw_1.Object.pafr_fecing[ll_new]  =	dw_6.Object.pafr_fecing[ll_fila]  
		dw_1.Object.pafr_fecdes[ll_new]  =	dw_6.Object.pafr_fecdes[ll_fila]  
		//dw_1.Object.pafr_cjssal[ll_new]  =	dw_6.Object.pafr_cjssal[ll_fila]  
		dw_1.Object.pafr_huert4[ll_new]  =	dw_6.Object.pafr_huert4[ll_fila]  
		dw_1.Object.pafr_cuart4[ll_new]  =	dw_6.Object.pafr_cuart4[ll_fila]  
		dw_1.Object.pafr_rotpak[ll_new]  =	dw_6.Object.pafr_rotpak[ll_fila]  
		dw_1.Object.pafr_docrel[ll_new]	= 	dw_6.Object.pafr_docrel[ll_fila]
	NEXT
END IF	
end subroutine

public subroutine actualiza_detalle_trans ();Long	ll_fila

FOR ll_fila = 1 TO dw_1.RowCount()
	dw_1.Object.espe_codigo[ll_fila] = dw_2.Object.espe_codigo[1]
	dw_1.Object.vari_codigo[ll_fila] = dw_2.Object.vari_codigo[1]
	dw_1.Object.emba_codigo[ll_fila] = dw_2.Object.emba_codigo[1]
	dw_1.Object.etiq_codigo[ll_fila] = dw_2.Object.etiq_codigo[1]
	dw_1.Object.pafr_fecemb[ll_fila] = dw_2.Object.paen_fecemb[1]
	dw_1.Object.cond_codigo[ll_fila] = dw_2.Object.cond_codigo[1]
NEXT

end subroutine

public function boolean existe_pallet_devolucion (string ls_columna);Integer	li_cliente, li_planta, li_Movto
Long		ll_nropal, ll_pcopda, ll_cantid, ll_PFruta
Boolean	lb_retorno = False

li_cliente					=	Integer(dw_2.GetItemNumber(1, "clie_codigo"))
li_planta					=	Integer(dw_2.GetItemNumber(1, "plde_codigo"))
ll_nropal 					=	Long(ls_columna)

SELECT	Count(*)
	INTO	:ll_cantid
	FROM	dba.palletencab
	WHERE	clie_codigo	= 	:li_cliente
	AND	paen_numero = 	:ll_nropal
	AND	plde_codigo	=	:li_planta
	AND	paen_estado = 	2;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
	lb_retorno = True
ELSEIF ll_cantid > 0 THEN
	//MessageBox("Atención","El Numero de Pallet Ingresado es un Ingreso Definitivo. Ingrese Otro.", Exclamation!, Ok!)
	//istr_mant.UsuarioSoloConsulta	=	True
	lb_retorno = False
ELSE
	MessageBox("Atención","El Numero de Pallet No existe o NO ha Sido Despachado de Esta Planta. Ingrese Otro.", Exclamation!, Ok!)
	istr_mant.UsuarioSoloConsulta	=	True
	lb_retorno = True
END IF	
	   
RETURN lb_retorno

end function

public function boolean existepallet_repa (string ls_columna);Integer	li_cliente, li_planta, li_Movto
Long		ll_nropal, ll_pcopda, ll_cantid, ll_PFruta
Boolean	lb_retorno = False

li_cliente					=	Integer(dw_2.GetItemNumber(1, "clie_codigo"))
li_planta					=	Integer(dw_2.GetItemNumber(1, "plde_codigo"))
ll_nropal 					=	Long(ls_columna)

SELECT	Count(*)
	INTO	:ll_cantid
	FROM	dba.repalletdeta
	WHERE	clie_codigo	= 	:li_cliente
	AND	(paen_numero = :ll_nropal
	OR    paen_nroori  = :ll_nropal) 
	AND	plde_codigo	=	:li_planta;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla repalletdeta")
	lb_retorno = True
ELSEIF ll_cantid > 0  and Integer(istr_mant.argumento[50]) <> 6  THEN
	MessageBox("Atención","El Número de Pallet Ingresado esta en un Repalletizado. Ingrese Otro.", Exclamation!, Ok!)
	istr_mant.UsuarioSoloConsulta	=	True
	lb_retorno = True		
END IF	
	   
RETURN lb_retorno

end function

public function boolean borra_detalle (long al_pallet);Integer	li_cliente, li_planta, li_Movto
Long		ll_nropal

li_cliente					=	Integer(dw_2.GetItemNumber(1, "clie_codigo"))
li_planta					=	Integer(dw_2.GetItemNumber(1, "plde_codigo"))
ll_nropal 					=	Long(al_pallet)

DELETE	dba.palletfruta_trans
 WHERE	clie_codigo	= 	:li_cliente
	AND	paen_numero = 	:ll_nropal
	AND	plde_codigo	=	:li_planta;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla palletfruta_trans")
	Return True

END IF	
	   
RETURN False
end function

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

pb_ins_det.Enabled = lb_estado
	
end subroutine

public function boolean existe_palletdespachado (long al_pallet);Integer	li_cliente, li_planta, li_Movto
Long		ll_nropal, ll_pcopda, ll_cantid, ll_PFruta
Boolean	lb_retorno = False

li_cliente					=	Integer(dw_2.GetItemNumber(1, "clie_codigo"))
li_planta					=	Integer(dw_2.GetItemNumber(1, "plde_codigo"))
ll_nropal 					=	Long(al_pallet)

SELECT	Count(*)
	INTO	:ll_cantid
	FROM	dba.despafrigode as det,dba.despafrigoen as enc
	WHERE	det.clie_codigo = :li_cliente
	AND	det.paen_numero = :ll_nropal
	AND	det.plde_codigo =	:li_planta
	AND   enc.defe_tiposa <> 11
	AND	det.clie_Codigo = enc.clie_codigo
	AND 	det.plde_codigo = enc.plde_codigo
	AND	det.defe_numero = enc.defe_numero;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla PalletEncab")
	lb_retorno = True
ELSEIF ll_cantid > 0  THEN
	MessageBox("Atención","El número de pallet se encuentra despachado a puerto. Ingrese otro.", Exclamation!, Ok!)
	istr_mant.UsuarioSoloConsulta	=	True
	lb_retorno = True		
END IF	
	   
RETURN lb_retorno

end function

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

This.Icon											=	Gstr_apl.Icono
//This.ParentWindow().ToolBarVisible			=	True
im_menu.Item[1].Item[6].Enabled				=	True
im_menu.Item[7].Visible							=	True

dw_2.Object.clie_codigo.Protect				=	1
dw_2.Object.plde_codigo.Protect				=	1
dw_2.Object.clie_codigo.BackGround.Color 	= 	RGB(166,180,210)
dw_2.Object.plde_codigo.BackGround.Color 	= 	RGB(166,180,210)

/* La asignación a las siguientes variables no se hereda, debe ser adicional en ventana descendiente */
istr_mant2.Argumento[20]	=	'0'
istr_mant2.Argumento[21]	=	''
istr_mant2.Argumento[25]	=	'0'
istr_mant2.Argumento[26]	=	'0'

istr_mant2						=	Message.PowerObjectParm

IF istr_mant2.Argumento[5] = 'dw_mues_repalletdeta' THEN
	dw_2.Object.paen_pmixto.Protect	=	0
ELSE
	dw_2.Object.paen_pmixto.Protect	=	1
END IF

dw_3.DataObject			=	istr_mant2.Argumento[5]
ii_CantPallets				=	Integer(istr_mant2.Argumento[11])
ii_CantPuchos				=	Integer(istr_mant2.Argumento[4])

IF ii_CantPallets=0 THEN

		dw_2.SetItem(1, "paen_tipopa", 2)
		dw_2.Object.paen_tipopa.Protect	=	1
		
END IF

istr_mant.Argumento[1]	=	istr_mant2.Argumento[3]
istr_mant.Argumento[20]	=	istr_mant2.Argumento[20] //tipo recepcion
istr_mant.Argumento[21]	=	istr_mant2.Argumento[21]
istr_mant.Argumento[22]	=	"1"		//	Pallet de Exportación
istr_mant.Argumento[23]	=	"0"		//	Pallet Mixto
istr_mant.Argumento[24]	=	istr_mant2.Argumento[24]
istr_mant.Argumento[15] =  istr_mant2.Argumento[8] //Lista de Productores de Pallets
istr_mant.argumento[17] =  istr_mant2.argumento[14]//tipo de pantalla; 1 = repalletizado
istr_mant.argumento[24] =  istr_mant2.argumento[15]//cuando es nuevo repalletizaje
istr_mant.argumento[44] =  istr_mant2.Argumento[24]

istr_mant.argumento[50] =  istr_mant2.Argumento[50]//tipo recepcion


IF istr_mant.argumento[50] = '1' THEN 
	dw_2.Object.cond_codigo.Protect	=	1
	dw_2.Object.cond_codigo.BackGround.Color = RGB(166,180,210)
ELSE
	dw_2.Object.cond_codigo.Protect	=	0
	dw_2.Object.cond_codigo.BackGround.Color = RGB(255,255,255)
END IF	

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

dw_3.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
dw_5.SetTransObject(sqlca)
dw_7.SetTransObject(sqlca)

istr_mant2.dw.ShareData(dw_3)

istr_mant.Argumento[16]=''

//dw_6.SetTransObject(sqlca)

iuo_variedadrotula			=	CREATE	uo_variedadrotula
pb_nuevo.TriggerEvent(Clicked!)
//GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
//							This.Title, "Acceso a Aplicación", 1)

end event

on w_maed_palletencab_recepcion_trans_inter.create
int iCurrent
call super::create
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_6=create dw_6
this.dw_5=create dw_5
this.dw_7=create dw_7
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_3
this.Control[iCurrent+2]=this.dw_4
this.Control[iCurrent+3]=this.dw_6
this.Control[iCurrent+4]=this.dw_5
this.Control[iCurrent+5]=this.dw_7
end on

on w_maed_palletencab_recepcion_trans_inter.destroy
call super::destroy
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_6)
destroy(this.dw_5)
destroy(this.dw_7)
end on

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
//
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_fila, ll_inspec, ll_prod,ll_cajas,ll_fil1, ll_productor, ll_Numero, ll_fila2
Integer  li_clien,li_especie,li_vari,li_planta, li_codpak , &
			li_cliente, li_variedad, li_etique, li_cond, li_Secuen, li_cont
String   ls_emba,ls_calibr, ls_embala, ls_calibre
Date     ld_fecemb

IF Message.DoubleParm = -1 THEN //RETURN
ELSE
	IF UpperBound(istr_mant2.Argumento) < 21 THEN
		istr_mant2.Argumento[21] = ''
	END IF
	
	IF UpperBound(istr_mant3.Argumento) < 2 THEN
		istr_mant3.Argumento[2] = ''
	END IF
	
	IF istr_mant2.argumento[20] <> '3' AND istr_mant2.argumento[20] <> '6' AND & 
		istr_mant2.argumento[20] <> '2'THEN
		IF dw_1.rowcount() > 0 THEN
			FOR ll_fil1=1 to dw_1.rowcount()
				 //IF istr_mant2.argumento[31] = '1' then
					 dw_1.Object.pafr_copack[ll_fil1] = Integer(istr_mant2.argumento[21])
				 //ELSE
				 //	 dw_1.Object.pafr_copack[ll_fil1] = -1
				 //END IF
			NEXT
		END IF
	END IF
	
	iuo_variedadrotula.existe(dw_2.Object.espe_codigo[1],dw_2.Object.vari_codigo[1], True, sqlca) 
	
	dw_2.Object.paen_varrot[1] = iuo_variedadrotula.Varirotula
	
	FOR ll_fila2 = 1 TO dw_1.RowCount()
		dw_1.Object.pafr_varrot[ll_fila2]	=	iuo_variedadrotula.Varirotula
	NEXT
		
	IF istr_mant2.Argumento[5] <> 'dw_mues_repalletdeta' THEN
		
		// Para guardar la fecha de Ingreso cuando es Packing
		IF istr_mant2.argumento[13] = '2' AND istr_mant2.argumento[39] <> '' THEN
			IF dw_1.rowcount() > 0 THEN
				FOR ll_fil1=1 to dw_1.rowcount()			
					 dw_1.Object.pafr_fecing[ll_fil1] = Date(istr_mant2.argumento[39])
				NEXT
			END IF
		END IF
		
		// Para guardar la fecha de Ingreso cuando traspaso Inperplanta
		IF istr_mant2.argumento[13] = '1' AND istr_mant2.argumento[39] <> '' THEN
			IF dw_1.rowcount() > 0 THEN
				FOR ll_fil1=1 to dw_1.rowcount()		
					 IF Isnull(dw_1.Object.pafr_fecing[ll_fil1]) OR dw_1.Object.pafr_fecing[ll_fil1] = Date('1900-01-01') THEN
						 dw_1.Object.pafr_fecing[ll_fil1] = Date(istr_mant2.argumento[39])
					END IF
				NEXT
			END IF
		END IF
	END IF
	
	IF Isnull(dw_2.Object.paen_fecemb[1]) THEN
		Messagebox("Atención","No ha Ingresado Fecha de Embalaje",exclamation!)
		dw_2.setcolumn("paen_fecemb")
		dw_2.setfocus()
		RETURN
	END IF
	
	IF dw_2.Object.paen_tipopa[1] = 1 THEN
		IF Isnull(dw_2.Object.tpem_codigo[1]) THEN
			Messagebox("Atención","No ha Ingresado Tipo Pallet",exclamation!)
			dw_2.setcolumn("tpem_codigo")
			dw_2.setfocus()
			Message.DoubleParm = -1
			RETURN
		END IF
	END IF	
	
	IF not existe_recprefriopda(dw_2.Object.clie_codigo[1],dw_2.Object.plde_codigo[1],dw_2.Object.paen_numero[1] ) THEN
		dw_2.SetItem(1, "cama_codigo", 0)
		dw_2.SetItem(1, "paen_calle", 1)
		dw_2.SetItem(1, "paen_base", 1)
		dw_2.SetItem(1, "paen_posici", 1)
		dw_2.SetItem(1, "tmvp_codigo", 0)
	ELSE
		dw_2.SetItem(1, "cama_codigo", i_vec_ubicacion[1])
		dw_2.SetItem(1, "paen_calle", i_vec_ubicacion[2])
		dw_2.SetItem(1, "paen_base", i_vec_ubicacion[3])
		dw_2.SetItem(1, "paen_posici", i_vec_ubicacion[4])
		dw_2.SetItem(1, "tmvp_codigo", i_vec_ubicacion[5])
	END IF	
	
	IF gb_Repalletizado THEN
		ll_fila	=	dw_3.InsertRow(0)
		dw_3.Object.plde_codigo[ll_fila]		=	dw_2.Object.plde_codigo[1]
		dw_3.Object.clie_codigo[ll_fila]		=	Integer(istr_mant.Argumento[1])
		dw_3.Object.paen_numero[ll_fila]		=	dw_2.Object.paen_numero[1]
		dw_3.Object.repe_numero[ll_fila]		=	Long(istr_mant2.Argumento[2])
		dw_3.Object.plde_codigo[ll_fila]		=	Integer(istr_mant2.Argumento[1])
		dw_3.Object.repd_tipood[ll_fila]		=	2
		dw_3.Object.vari_nombre[ll_fila]		=	dw_2.Object.vari_nombre[1]
		dw_3.Object.emba_codigo[ll_fila]		=	dw_2.Object.emba_codigo[1]
		dw_3.Object.cate_codigo[ll_fila]		=	dw_2.Object.cate_codigo[1]
		dw_3.Object.stat_codigo[ll_fila]		=	dw_2.Object.stat_codigo[1]
		dw_3.Object.espe_codigo[ll_fila]		=	dw_2.Object.espe_codigo[1]			
		dw_3.Object.vari_codigo[ll_fila]		=	dw_2.Object.vari_codigo[1]
		dw_3.Object.etiq_codigo[ll_fila]		=	dw_2.Object.etiq_codigo[1]
		dw_3.Object.trat_codigo[ll_fila]		=	dw_2.Object.trat_codigo[1]
		dw_3.Object.frio_codigo[ll_fila]		=	dw_2.Object.frio_codigo[1]
		dw_3.Object.cond_codigo[ll_fila]		=	dw_2.Object.cond_codigo[1]
		dw_3.Object.paen_fecemb[ll_fila]		=	dw_2.Object.paen_fecemb[1]
		dw_3.Object.paen_cosecha[ll_fila]	=	dw_2.Object.paen_cosecha[1]
		dw_3.Object.paen_altura[ll_fila]		=	dw_2.Object.paen_altura[1]
		dw_3.Object.copa_codigo[ll_fila]		=	dw_2.Object.copa_codigo[1]
		dw_3.Object.tiem_codigo[ll_fila]		=	dw_2.Object.tiem_codigo[1]
		dw_3.Object.paen_tipopa[ll_fila]		=	dw_2.Object.paen_tipopa[1]
		dw_3.Object.tpem_codigo[ll_fila]		=	dw_2.Object.tpem_codigo[1]
		dw_3.Object.paen_ccajas[ll_fila]		=	dw_2.Object.paen_ccajas[1]	
		
		IF istr_mant2.Argumento[5] = 'dw_mues_repalletdeta' THEN
			dw_3.Object.pafr_secuen[ll_fila]		=	ll_fila	
		END IF
		
		//IF istr_mant.argumento[20] <> '6' THEN 
			IF dw_2.Object.paen_inspec[1] = 1 THEN
				ll_inspec	=	Long(String(Long(istr_mant3.Argumento[1]),'00000'))
				IF duplicadoinspecdet(ll_inspec)=False THEN
					dw_4.Object.inpe_tipoin[ll_fila] = 1
					dw_4.Object.inpe_numero[ll_fila] = ll_inspec
					dw_4.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[1])
					dw_4.Object.plde_codigo[ll_fila] = Integer(istr_mant2.Argumento[1])
					dw_4.Object.inpe_secuen[ll_fila] = 1
					dw_4.Object.paen_numero[ll_fila] = dw_2.Object.paen_numero[1]
					dw_4.Object.inpd_fechai[ll_fila] = Date(istr_mant3.Argumento[2])
					dw_4.Object.dest_codigo[ll_fila] = dw_2.Object.dest_codigo[1]
					dw_2.Object.inpe_numero[1]			= ll_inspec
					dw_2.Object.inpe_tipoin[1]			= 1
					dw_2.Object.inpe_fechai[1]			= Date(istr_mant3.Argumento[2])
					dw_4.Object.inpd_fecres[ll_fila]	= Date(istr_mant3.Argumento[2])
					
				END IF
				IF duplicadoinspeccion(ll_inspec)=False THEN
					dw_5.Object.inpe_tipoin[1] = 1
					dw_5.Object.inpe_numero[1] = ll_inspec
					dw_5.Object.clie_codigo[1] = Integer(istr_mant.Argumento[1])
					dw_5.Object.plde_codigo[1] = Integer(istr_mant2.Argumento[1])
					dw_5.Object.inpe_secuen[1] = 1
					dw_5.Object.inpe_fechai[1] = Date(istr_mant3.Argumento[2])			
					dw_5.Object.inpe_desdet[1] = istr_mant3.argumento[6]			
					dw_5.Object.inpe_fecres[1] = Date(istr_mant3.Argumento[2])			
					dw_5.Object.inpe_dessec[1] = Integer(istr_mant3.argumento[7])	
					dw_5.Object.espe_codigo[1] = Integer(istr_mant.argumento[3])
					dw_5.Object.dest_codigo[1] = dw_2.Object.dest_codigo[1]
				END IF			
			END IF		
		//END IF
	ELSE
		IF NoExisteTarja(Integer(istr_mant.Argumento[1]), Integer(istr_mant2.Argumento[1]), &
							Long(istr_mant2.Argumento[2]), dw_2.Object.paen_numero[1]) THEN	
			//	Actualiza Planta y Estado Existencia en caso de ser Recepción Interplanta
			dw_2.SetItem(1, "plde_codigo", Integer(istr_mant2.argumento[1]))
			dw_2.SetItem(1, "paen_estado", 1)
			
			//	Actualiza Usuario
			dw_2.SetItem(1,"paen_pcopda",1)
			dw_2.SetItem(1,"paen_usuari",gstr_us.Nombre)
			dw_2.SetItem(1,"paen_estaci",gstr_us.Computador)
	
			//	Cambio en Condición
			FOR ll_Fila = 1 TO dw_1.RowCount()
				dw_1.SetItem(ll_Fila, "plde_codigo", dw_2.Object.plde_codigo[1])
				dw_1.SetItem(ll_Fila, "cond_codigo", dw_2.Object.cond_codigo[1])			
			NEXT
	
			ll_fila	=	dw_3.InsertRow(0)
			IF IsNull(istr_mant2.Argumento[6]) OR istr_mant2.Argumento[6] = "" THEN
				dw_3.Object.plde_codigo[ll_fila]		=	dw_2.Object.plde_codigo[1]
				dw_3.Object.rfpe_numero[ll_fila]		=	Long(istr_mant2.Argumento[2])
				dw_3.Object.clie_codigo[ll_fila]		=	dw_2.Object.clie_codigo[1]
				dw_3.Object.paen_numero[ll_fila]		=	dw_2.Object.paen_numero[1]
				dw_3.Object.rfpe_pcopda[ll_Fila]		=	1
				dw_3.Object.vari_nombre[ll_fila]		=	dw_2.Object.vari_nombre[1]
				dw_3.Object.emba_codigo[ll_fila]		=	dw_2.Object.emba_codigo[1]
				dw_3.Object.cate_codigo[ll_fila]		=	dw_2.Object.cate_codigo[1]
				dw_3.Object.stat_codigo[ll_fila]		=	dw_2.Object.stat_codigo[1]
				dw_3.Object.espe_codigo[ll_fila]		=	dw_2.Object.espe_codigo[1]			
				dw_3.Object.vari_codigo[ll_fila]		=	dw_2.Object.vari_codigo[1]
				dw_3.Object.etiq_codigo[ll_fila]		=	dw_2.Object.etiq_codigo[1]
				dw_3.Object.trat_codigo[ll_fila]		=	dw_2.Object.trat_codigo[1]
				dw_3.Object.frio_codigo[ll_fila]		=	dw_2.Object.frio_codigo[1]
				dw_3.Object.cond_codigo[ll_fila]		=	dw_2.Object.cond_codigo[1]
				dw_3.Object.paen_fecemb[ll_fila]		=	dw_2.Object.paen_fecemb[1]
				dw_3.Object.paen_cosecha[ll_fila]	=	dw_2.Object.paen_cosecha[1]
				dw_3.Object.paen_altura[ll_fila]		=	dw_2.Object.paen_altura[1]
				dw_3.Object.copa_codigo[ll_fila]		=	dw_2.Object.copa_codigo[1]
				dw_3.Object.tiem_codigo[ll_fila]		=	dw_2.Object.tiem_codigo[1]
				dw_3.Object.paen_tipopa[ll_fila]		=	dw_2.Object.paen_tipopa[1]
				dw_3.Object.tpem_codigo[ll_fila]		=	dw_2.Object.tpem_codigo[1]
				dw_3.Object.paen_ccajas[ll_fila]		=	dw_2.Object.paen_ccajas[1]			
			END IF
			
		//	IF istr_mant.argumento[20] <> '6' THEN
				IF dw_2.Object.paen_inspec[1] = 1 THEN
					ll_inspec	=	Long(String(Integer(istr_mant2.Argumento[21]),'00') + String(Long(istr_mant3.Argumento[1]),'0000'))
					IF duplicadoinspecdet(ll_inspec)=False THEN			
						dw_4.Object.inpe_tipoin[ll_fila] = 1
						dw_4.Object.inpe_numero[ll_fila] = ll_inspec
						dw_4.Object.clie_codigo[ll_fila] = Integer(istr_mant.Argumento[1])
						dw_4.Object.plde_codigo[ll_fila] = Integer(istr_mant2.Argumento[1])
						dw_4.Object.inpe_secuen[ll_fila] = 1
						dw_4.Object.paen_numero[ll_fila] = dw_2.Object.paen_numero[1]
						dw_4.Object.inpd_fechai[ll_fila] = Date(istr_mant3.Argumento[2])
						dw_4.Object.dest_codigo[ll_fila] = dw_2.Object.dest_codigo[1]	
						dw_2.Object.inpe_numero[1]			= ll_inspec
						dw_2.Object.inpe_tipoin[1]			= 1
						dw_2.Object.inpe_fechai[1]			= Date(istr_mant3.Argumento[2])
						dw_4.Object.inpd_fecres[ll_fila]	= Date(istr_mant3.Argumento[2])
					END IF
					IF duplicadoinspeccion(ll_inspec)=False THEN
						dw_5.Object.inpe_tipoin[1] = 1
						dw_5.Object.inpe_numero[1] = ll_inspec
						dw_5.Object.clie_codigo[1] = Integer(istr_mant.Argumento[1])
						dw_5.Object.plde_codigo[1] = Integer(istr_mant2.Argumento[1])
						dw_5.Object.inpe_secuen[1] = 1
						dw_5.Object.inpe_fechai[1] = Date(istr_mant3.Argumento[2])	
						dw_5.Object.inpe_desdet[1] = istr_mant3.argumento[6]			
						dw_5.Object.inpe_fecres[1] = Date(istr_mant3.Argumento[2])			
						dw_5.Object.inpe_dessec[1] = Integer(istr_mant3.argumento[7])	
						dw_5.Object.espe_codigo[1] = Integer(istr_mant.argumento[3])	
						dw_5.Object.dest_codigo[1] = dw_2.Object.dest_codigo[1]
					END IF			
				END IF
			//END IF	
			IF	dw_6.RowCount()	>	0	AND istr_mant2.Argumento[40] = '1' THEN
				FOR	ll_fila	=	1	TO	dw_6.RowCount()			
					li_cliente	=	dw_6.GetItemNumber(ll_fila,'clie_codigo')
					ll_Numero	=	dw_6.GetItemNumber(ll_fila,'paen_numero')
					li_especie	=	dw_6.GetItemNumber(ll_fila,'espe_codigo')
					li_variedad	=	dw_6.GetItemNumber(ll_fila,'vari_codigo')
					ls_embala	=	dw_6.GetItemString(ll_fila,'emba_codigo')
					ll_productor=	dw_6.GetItemNumber(ll_fila,'prod_codigo')
					li_cond		=	dw_6.GetItemNumber(ll_fila,'cond_codigo')
					li_etique	=	dw_6.GetItemNumber(ll_fila,'etiq_codigo')				
					li_planta	=	dw_6.GetItemNumber(ll_fila,'plde_codigo')				
					ls_calibre	=	dw_6.GetItemString(ll_fila,'pafr_calibr')				
					//li_Secuen	=	dw_6.GetItemNumber(ll_fila,'pafr_secuen')
			
					ll_fil1 = dw_1.Find("clie_codigo   = "+String(li_cliente) + &
											  " AND paen_numero= "+String(ll_Numero) + &
											  " AND espe_codigo= "+String(li_especie) + &
											  " AND vari_codigo= "+String(li_variedad) +&
											  " AND emba_codigo= '"+ls_embala + "'"	 +&
											  " AND prod_codigo= "+String(ll_productor)+ &
											  " AND cond_codigo= "+String(li_cond) + &										  
											  " AND etiq_codigo= "+String(li_etique) + &
											  " AND plde_codigo= "+String(li_planta) + &
											  " AND pafr_calibr= '"+ ls_calibre+ "'" ,1,dw_1.RowCount())	
											  
					IF ll_fil1 > 0 THEN
						dw_1.Object.pafr_fecemb[ll_fil1]	=	dw_6.Object.pafr_fecemb[ll_fila]
					END IF				
				NEXT			
			END IF
		END IF 
	END IF
END IF

ll_numero = dw_2.Object.paen_numero[1]
li_cliente = dw_2.Object.clie_Codigo[1]
li_planta = dw_2.Object.plde_codigo[1]	

SELECT count()
INTO :li_cont
FROM dba.palletencab_trans
WHERE clie_codigo = :li_cliente
AND plde_codigo = :li_planta
AND paen_numero = :ll_numero;

IF li_cont > 0 THEN
	borra_detalle(dw_2.Object.paen_numero[1])
END IF



end event

event ue_guardar;call super::ue_guardar;//grabadocrel()
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
		dw_2.Enabled			=	True
		pb_Eliminar.Enabled	=	True
		pb_Grabar.Enabled	=	True
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

IF istr_mant.argumento[20] <> '6' THEN
	pb_ins_det.Enabled	= False
ELSE	
	pb_ins_det.Enabled	= True
END IF	

dw_2.SetColumn("paen_fecemb")
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
		
		IF istr_mant2.Argumento[40] = '1' OR istr_mant.argumento[50] = '5' THEN	
			OpenWithParm(w_mant_deta_palletfruta_interfecemb_trans, istr_mant)
		ELSE	
			OpenWithParm(iw_mantencion, istr_mant)
		END IF	
	END IF
END IF
end event

event ue_nuevo;//Boolean	lb_TerminaPallet, lb_terminaPucho
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
dw_5.Reset()

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

dw_2.Object.paen_numero.Protect	=	0
dw_2.Object.paen_numero.BackGround.Color = RGB(255,255,255)
dw_2.SetRedraw(True)

dw_2.SetFocus()
dw_2.SetColumn("paen_numero")
dw_2.Setitem(1,"clie_codigo", Integer(istr_mant.argumento[1]))
dw_2.SetItem(1,"plde_codigo", Integer(istr_mant2.argumento[1]))
dw_2.SetItem(1,"espe_codigo", gi_CodEspecie)

IF istr_mant.argumento[17] = '1'and istr_mant.argumento[24] <> '1' THEN
	dw_2.SetItem(1,"paen_fecemb",Date(istr_mant.argumento[28]))
	dw_2.SetItem(1,"paen_cosecha",Date(istr_mant.argumento[28]))
END IF	

istr_mant.argumento[3]	=	String(gi_CodEspecie)
istr_mant.argumento[6]	= 	istr_mant2.argumento[1]
istr_mant.argumento[9]	= 	"1"
istr_mant.argumento[10]	=	"0"

IF ii_CantPallets=0 THEN
	dw_2.SetItem(1, "paen_tipopa", 2)
	dw_2.Object.paen_tipopa.Protect	=	1
	lb_TerminaPallet						=	True
END IF

IF dw_3.RowCount() > 0 THEN
	li_Pallets	=	dw_3.GetItemNumber(1, "pallets")
	li_Puchos	=	dw_3.GetItemNumber(1, "puchos")

	IF ii_CantPallets > 0 AND dw_3.GetItemNumber(1, "pallets") = ii_CantPallets THEN
		IF Not lb_MensPallet THEN
			MessageBox("Atención", "Se ha Completado la Cantidad de Pallets~r" + &
							"indicados en la Recepción")
			
			lb_MensPallet	=	True
		END IF
		
		dw_2.SetItem(1, "paen_tipopa", 2)
		
		dw_2.Object.paen_tipopa.Protect	=	1
		lb_TerminaPallet						=	True
	END IF
	
	IF ii_CantPuchos > 0 AND dw_3.GetItemNumber(1, "puchos") = ii_CantPuchos THEN
		IF Not lb_MensPucho THEN
			MessageBox("Atención", "Se ha Completado la Cantidad de Puchos~r" + &
							"indicados en la Recepción")
			
			lb_MensPucho	=	True
		END IF
		
		dw_2.SetItem(1, "paen_tipopa", 1)
		
		dw_2.Object.paen_tipopa.Protect	=	1
		lb_TerminaPucho						=	True
	END IF
	
	IF lb_TerminaPallet AND lb_TerminaPucho THEN
		pb_salir.TriggerEvent(Clicked!)
	ELSE
		istr_mant.argumento[3]	=	String(dw_2.Object.espe_codigo[1])
		istr_mant.argumento[6]	=	istr_mant2.argumento[1]
		istr_mant.argumento[9]	=	"1"
		istr_mant.argumento[10]	=	"0"
		
		IF Long(istr_mant2.argumento[6]) <> 0 THEN
			dw_2.SetFocus()
			dw_2.SetColumn("paen_numero")
			//dw_2.Setitem(1,"paen_numero", Long(istr_mant2.argumento[6]))	
			//dw_2.AcceptText()
			//dw_2.PostEvent(itemchanged!)
		END IF
	   ll_dataw3	=	dw_3.RowCount()
		IF ll_dataw3 > 0 THEN
			
			 dw_2.Setitem(1,"plde_codigo", dw_3.Object.plde_codigo[ll_dataw3])
			 istr_mant.argumento[6]		=	String(dw_3.Object.plde_codigo[ll_dataw3])
			 //dw_2.Setitem(1,"emba_codigo", dw_3.Object.emba_codigo[ll_dataw3])
			 //istr_mant.argumento[7]		=	dw_3.Object.emba_codigo[ll_dataw3]
			 dw_2.Setitem(1,"stat_codigo", dw_3.Object.stat_codigo[ll_dataw3])
			 dw_2.Setitem(1,"espe_codigo", dw_3.Object.espe_codigo[ll_dataw3])
			 istr_mant.argumento[3]		=	String(dw_3.Object.espe_codigo[ll_dataw3])
			 dw_2.Setitem(1,"vari_codigo", dw_3.Object.vari_codigo[ll_dataw3])
			 istr_mant.argumento[4]		=	String(dw_3.Object.vari_codigo[ll_dataw3])
			 dw_2.Setitem(1,"etiq_codigo", dw_3.Object.etiq_codigo[ll_dataw3])
			 istr_mant.argumento[9]		=	String(dw_3.Object.etiq_codigo[ll_dataw3])
			 dw_2.Setitem(1,"trat_codigo", dw_3.Object.trat_codigo[ll_dataw3])
			 dw_2.Setitem(1,"frio_codigo", dw_3.Object.frio_codigo[ll_dataw3])
			 dw_2.Setitem(1,"cond_codigo", dw_3.Object.cond_codigo[ll_dataw3])
			 istr_mant.argumento[10]	=	String(dw_3.Object.cond_codigo[ll_dataw3])
			 dw_2.Setitem(1,"paen_fecemb", dw_3.Object.paen_fecemb[ll_dataw3])
			 istr_mant.argumento[40]	=	String(dw_3.Object.paen_fecemb[ll_dataw3])		 
			 dw_2.Setitem(1,"paen_cosecha", dw_3.Object.paen_cosecha[ll_dataw3])
			 dw_2.Setitem(1,"paen_altura", dw_3.Object.paen_altura[ll_dataw3])
			 dw_2.Setitem(1,"copa_codigo", dw_3.Object.copa_codigo[ll_dataw3])
			 dw_2.Setitem(1,"cate_codigo", dw_3.Object.cate_codigo[ll_dataw3])
			 IF lb_MensPallet = False THEN
				dw_2.Setitem(1,"tiem_codigo", dw_3.Object.tiem_codigo[ll_dataw3])			
				dw_2.Setitem(1,"paen_tipopa", dw_3.Object.paen_tipopa[ll_dataw3])			
				dw_2.Setitem(1,"tpem_codigo", dw_3.Object.tpem_codigo[ll_dataw3])			
				dw_2.Setitem(1,"paen_ccajas", dw_3.Object.paen_ccajas[ll_dataw3])			
			 END IF
		END IF
	END IF
END IF
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
	
	istr_mant.Argumento[22]	=	String(dw_2.Object.paen_pexpor[1])
	istr_mant.Argumento[23]	=	String(dw_2.Object.paen_pmixto[1])
	istr_mant.argumento[40] =  String(dw_2.Object.paen_fecemb[1])
	
	istr_mant.argumento[3]	=	String(dw_2.Object.espe_codigo[1])
	istr_mant.argumento[4]	=	String(dw_2.Object.vari_codigo[1])
	istr_mant.argumento[5]	= 	dw_2.Object.vari_nombre[1]
	istr_mant.argumento[6]	= 	String(dw_2.Object.plde_codigo[1])
	istr_mant.argumento[7]	= 	dw_2.Object.emba_codigo[1]
	istr_mant.argumento[8]	= 	dw_2.Object.emba_nombre[1]
	istr_mant.argumento[9]	= 	String(dw_2.Object.etiq_codigo[1])
	istr_mant.argumento[10] = 	String(dw_2.Object.cond_codigo[1])
	istr_mant.argumento[12] = 	""
	
	OpenWithParm(w_mant_deta_palletfruta_repa, istr_mant)
	
	IF dw_1.RowCount() > 0 THEN HabilitaEncab(False)
	
	IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
		pb_eliminar.Enabled	= TRUE
		pb_grabar.Enabled		= TRUE
	END IF
	
	dw_1.SetRow(il_fila)
	dw_1.SelectRow(il_fila,True)
ELSEIF istr_mant2.Argumento[40] = '1' OR istr_mant.argumento[50] = '5' THEN	
	istr_mant.borra	= False
	istr_mant.agrega	= True
	cuentacajas()
	istr_mant.argumento[28]=String(dw_2.object.paen_fecemb[1])
	
	istr_mant.Argumento[22]	=	String(dw_2.Object.paen_pexpor[1])
	istr_mant.Argumento[23]	=	String(dw_2.Object.paen_pmixto[1])
	istr_mant.argumento[40] =  String(dw_2.Object.paen_fecemb[1])
	
	istr_mant.argumento[3]	=	String(dw_2.Object.espe_codigo[1])
	istr_mant.argumento[4]	=	String(dw_2.Object.vari_codigo[1])
	istr_mant.argumento[5]	= 	dw_2.Object.vari_nombre[1]
	istr_mant.argumento[6]	= 	String(dw_2.Object.plde_codigo[1])
	istr_mant.argumento[7]	= 	dw_2.Object.emba_codigo[1]
	istr_mant.argumento[8]	= 	dw_2.Object.emba_nombre[1]
	istr_mant.argumento[9]	= 	String(dw_2.Object.etiq_codigo[1])
	istr_mant.argumento[10] = 	String(dw_2.Object.cond_codigo[1])
	istr_mant.argumento[12] = 	""
	
	OpenWithParm(w_mant_deta_palletfruta_interfecemb_trans, istr_mant)
	
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

event ue_recuperadatos;/*En este script se debe extender el código, controlando la recuperacion de datos de dw_1 */
Integer	ll_fila_enc
w_main.SetMicroHelp("Recuperando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

pb_eliminar.Enabled		= False
pb_grabar.Enabled			= False
pb_eli_det.Enabled		= False

Long	ll_fila_d, ll_fila_e, respuesta
DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_fila_e	= dw_7.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[6]), Long(istr_mant.argumento[2]))
	ll_fila_enc	= dw_2.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]),Integer(istr_mant.argumento[6]))
	
	IF ll_fila_e = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos 1.", &
										Information!, RetryCancel!)
	ELSE
		IF ll_fila_e > 0 AND ll_fila_enc = 0 THEN
			dw_2.Object.clie_codigo[1] = dw_7.Object.clie_codigo[1] 
			dw_2.Object.paen_numero[1] = dw_7.Object.paen_numero[1]   
			dw_2.Object.paen_tipopa[1] = dw_7.Object.paen_tipopa[1]   
			dw_2.Object.espe_codigo[1] = dw_7.Object.espe_codigo[1]   
			dw_2.Object.vari_codigo[1] = dw_7.Object.vari_codigo[1]   
			dw_2.Object.tiem_codigo[1] = dw_7.Object.tiem_codigo[1]   
			dw_2.Object.emba_codigo[1] = dw_7.Object.emba_codigo[1]   
			dw_2.Object.cate_codigo[1] = dw_7.Object.cate_codigo[1]   
			dw_2.Object.etiq_codigo[1] = dw_7.Object.etiq_codigo[1]   
			dw_2.Object.stat_codigo[1] = dw_7.Object.stat_codigo[1]   
			dw_2.Object.trat_codigo[1] = dw_7.Object.trat_codigo[1]   
			dw_2.Object.frio_codigo[1] = dw_7.Object.frio_codigo[1]   
			dw_2.Object.cond_codigo[1] = dw_7.Object.cond_codigo[1]   
			dw_2.Object.plde_codigo[1] = dw_7.Object.plde_codigo[1]   
			dw_2.Object.paen_cosecha[1] = dw_7.Object.paen_cosecha[1]   
			dw_2.Object.paen_altura[1] = dw_7.Object.paen_altura[1]   
			dw_2.Object.tmvp_codigo[1] = dw_7.Object.tmvp_codigo[1]   
			dw_2.Object.paen_fecini[1] = dw_7.Object.paen_fecini[1]   
			dw_2.Object.paen_horain[1] = dw_7.Object.paen_horain[1]   
			dw_2.Object.cama_codigo[1] = dw_7.Object.cama_codigo[1]   
			dw_2.Object.paen_calle[1] = dw_7.Object.paen_calle[1]   
			dw_2.Object.paen_base[1] = dw_7.Object.paen_base[1]   
			dw_2.Object.paen_posici[1] = dw_7.Object.paen_posici[1]   
			dw_2.Object.paen_estado[1] = 1//dw_7.Object.paen_estado[1]   
			dw_2.Object.paen_fecemb[1] = dw_7.Object.paen_fecemb[1]   
			dw_2.Object.vari_nombre[1] = dw_7.Object.vari_nombre[1]   
			dw_2.Object.emba_nombre[1] = dw_7.Object.emba_nombre[1]   
			dw_2.Object.tpem_codigo[1] = dw_7.Object.tpem_codigo[1]   
			dw_2.Object.paen_ccajas[1] = dw_7.Object.paen_ccajas[1]   
			dw_2.Object.paen_concal[1] = dw_7.Object.paen_concal[1]   
			dw_2.Object.paen_inspec[1] = dw_7.Object.paen_inspec[1]   
			dw_2.Object.dest_codigo[1] = dw_7.Object.dest_codigo[1]   
			dw_2.Object.paen_pexpor[1] = dw_7.Object.paen_pexpor[1]   
			dw_2.Object.paen_pmixto[1] = dw_7.Object.paen_pmixto[1]   
			dw_2.Object.paen_varrot[1] = dw_7.Object.paen_varrot[1]   
			dw_2.Object.paen_nrasda[1] = dw_7.Object.paen_nrasda[1]   
			dw_2.Object.copa_codigo[1] = dw_7.Object.copa_codigo[1]   
			dw_2.Object.prod_codigo[1] = dw_7.Object.prod_codigo[1]   
			dw_2.Object.paen_calibr[1] = dw_7.Object.paen_calibr[1]   
			dw_2.Object.paen_pcopda[1] = dw_7.Object.paen_pcopda[1]   
			dw_2.Object.paen_usuari[1] = dw_7.Object.paen_usuari[1]   
			dw_2.Object.paen_estaci[1] = dw_7.Object.paen_estaci[1]   
			dw_2.Object.paen_usumod[1] = dw_7.Object.paen_usumod[1]   
			dw_2.Object.paen_estmod[1] = dw_7.Object.paen_estmod[1]   
			dw_2.Object.inpe_tipoin[1] = dw_7.Object.inpe_tipoin[1]   
			dw_2.Object.inpe_fechai[1] = dw_7.Object.inpe_fechai[1]   
			dw_2.Object.inpe_numero[1] = dw_7.Object.inpe_numero[1]     
						
		
		END IF
		DO
//			ll_fila_d	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), Long(istr_mant.argumento[2]), &
//									Integer(istr_mant.argumento[6]))
//									
			
			ll_fila_d = 1

			IF ll_fila_d = -1 THEN
				respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos 2.", &
												Information!, RetryCancel!)
			ELSE
				pb_imprimir.Enabled	= True
				IF (Integer(istr_mant.argumento[20]) <> 3 AND &
					(ii_estado=2 OR ii_estado=3)) THEN
					istr_mant.solo_consulta	=	True	
				ELSE
					istr_mant.solo_consulta	=	False
					pb_eliminar.Enabled		= True
					pb_grabar.Enabled		= True				
					pb_ins_det.Enabled		= True
				END IF
				IF ll_fila_d > 0 THEN
					IF (Integer(istr_mant.argumento[20]) <> 3 AND &
						(ii_estado=2 OR ii_estado=3)) THEN
						pb_eli_det.Enabled = False
					ELSE
						
//						IF Integer(istr_mant.argumento[20]) = 3 THEN
//							dw_1.reset()
//							Inserta_devolucion()
//						END IF	
											
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

istr_mant.Argumento[22]	=	String(dw_2.Object.paen_pexpor[1])
istr_mant.Argumento[23]	=	String(dw_2.Object.paen_pmixto[1])
istr_mant.argumento[40] =  String(dw_2.Object.paen_fecemb[1])

istr_mant.argumento[3]	=	String(dw_2.Object.espe_codigo[1])
istr_mant.argumento[4]	=	String(dw_2.Object.vari_codigo[1])
istr_mant.argumento[5]	= 	dw_2.Object.vari_nombre[1]
istr_mant.argumento[6]	= 	String(dw_2.Object.plde_codigo[1])
istr_mant.argumento[7]	= 	dw_2.Object.emba_codigo[1]
istr_mant.argumento[8]	= 	dw_2.Object.emba_nombre[1]
istr_mant.argumento[9]	= 	String(dw_2.Object.etiq_codigo[1])
istr_mant.argumento[10] = 	String(dw_2.Object.cond_codigo[1])
istr_mant.argumento[12] = 	""

dw_1.Enabled				=	True
pb_ins_det.Enabled		=	True
dw_2.GetChild("tpem_codigo", dw_emba)
dw_emba.SetTransObject(sqlca)
IF dw_emba.Retrieve(dw_2.Object.clie_codigo[1], dw_2.Object.emba_codigo[1]) = 0 THEN
	dw_emba.Insertrow(0)
END IF	

end event

event ue_seleccion;//
end event

type dw_1 from w_maed_palletencab_trans`dw_1 within w_maed_palletencab_recepcion_trans_inter
integer x = 87
boolean righttoleft = false
end type

type dw_2 from w_maed_palletencab_trans`dw_2 within w_maed_palletencab_recepcion_trans_inter
integer y = 60
end type

event dw_2::itemchanged;Long		ll_null, ll_fila
String	ls_columna, ls_asda
Date		ld_nula

SetNull(ld_nula)

SetNull(ll_null)
ls_columna = GetColumnName()

CHOOSE CASE ls_columna

	CASE "paen_inspec"
		IF Integer(data) = 1 THEN
			istr_mant3.argumento[3] = istr_mant.argumento[3]
			OpenWithParm(w_proc_inspeccion_informada, istr_mant3)
			istr_mant3 = Message.PowerObjectParm
			istr_mant3.argumento[26] 	= istr_mant3.argumento[5] //destino
			dw_2.Object.dest_codigo[1] = Integer(istr_mant3.argumento[5])
		END IF

	CASE "paen_numero"
			
		IF Duplicado(Data) THEN
			dw_2.SetItem(1, "paen_numero", ll_null)
			RETURN 1
		END IF
		
		ExisteOtraPlanta(Long(Data))
			
		IF existepallet_repa(data) THEN
			dw_2.SetItem(1, "paen_numero", ll_null)
			RETURN 1
		END IF	
		
		IF existepallet_definitivo(data) THEN
			dw_2.SetItem(1, "paen_numero", ll_null)
			RETURN 1
		END IF	
		
		IF existe_palletdespachado(Long(data)) THEN
			dw_2.SetItem(1, "paen_numero", ll_null)
			RETURN 1
		END IF	
		
		istr_mant.argumento[2] = data
		
		IF ii_existe > 0 THEN
			Parent.TriggerEvent("ue_recuperadatos")
			
		END IF	

END CHOOSE

//call super::itemchanged

CHOOSE CASE ls_columna

	CASE "vari_codigo"
		IF istr_mant.argumento[17] = '1'and istr_mant.argumento[24] <> '1' THEN
			IF Pos(istr_mant.argumento[27], "," + Data) = 0 THEN
				messagebox("Atención","Variedad no corresponde a Pallets originales", Exclamation!, Ok!)
				dw_2.SetItem(row, "vari_codigo", ll_null)
				RETURN 1
			ELSE
				IF dw_1.RowCount() > 0 THEN
					actualiza_detalle_trans()
				END IF
			END IF
		END IF
		
	CASE "paen_fecemb"
		istr_mant.Argumento[40] = Data
		IF dw_1.RowCount() > 0 THEN
			actualiza_detalle_trans()
		END IF	
		
	CASE "emba_codigo"
		IF dw_1.RowCount() > 0 THEN
			actualiza_detalle_trans()
		END IF
		
		dw_2.GetChild("tpem_codigo", dw_emba)
		dw_emba.SetTransObject(sqlca)
		IF dw_emba.Retrieve(dw_2.Object.clie_codigo[1], data) = 0 THEN
			dw_emba.Insertrow(0)
		END IF	
				
	CASE "espe_codigo"
		IF dw_1.RowCount() > 0 THEN
			actualiza_detalle_trans()
		END IF
	
	CASE "etiq_codigo"
		IF dw_1.RowCount() > 0 THEN
			actualiza_detalle_trans()
		END IF	
		
	CASE "cond_codigo"
		IF dw_1.RowCount() > 0 THEN
			actualiza_detalle_trans()	
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
		END IF
					
	CASE "plde_codigo"
		istr_mant.argumento[6]	= data
	
	CASE "paen_tipopa"
		IF data = '2' THEN
			dw_2.SetItem(1, "tpem_codigo", String(ll_null))
		END IF	
		
	CASE "emba_codigo"
		IF ExisteEmbalaje(data) = False THEN
			dw_2.SetItem(1, "emba_nombre", "")
			dw_2.SetItem(1, "emba_codigo", ll_null)
			RETURN 1
		ELSE
		 dw_emba.Retrieve(Integer(istr_mant.argumento[1]),istr_mant.argumento[7])
		 dw_2.SetItem(1, "tpem_codigo", string(ll_null))
		END IF
			
	CASE "tpem_codigo"
		IF dw_2.object.paen_tipopa[Row] = 1 THEN
			IF ExisteTipoEmbalaje(data) = False THEN
				dw_2.SetItem(1, "emba_codigo", string(ll_null))
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
		
	CASE "paen_nrasda"
		IF data<>'' OR NOT IsNull(data)  THEN
			istr_mant.argumento[32] = 'RIOBL'+ data
			data = istr_mant.argumento[32]
		END IF
		
		IF Existe_nrasda(data) OR IsNull(data) THEN
			dw_2.SetItem(1, "paen_nrasda", String(ll_null))
			RETURN 1
		END IF	
END CHOOSE

IF ii_yaexiste = 1 THEN
	ii_yaexiste = 0
	dw_2.SetItem(1, "paen_numero", ll_null)
	RETURN 1
END IF
IF ii_existe = 0 THEN
	HabilitaIngreso()
END IF
end event

type pb_nuevo from w_maed_palletencab_trans`pb_nuevo within w_maed_palletencab_recepcion_trans_inter
integer weight = 400
fontcharset fontcharset = ansi!
end type

type pb_eliminar from w_maed_palletencab_trans`pb_eliminar within w_maed_palletencab_recepcion_trans_inter
end type

type pb_grabar from w_maed_palletencab_trans`pb_grabar within w_maed_palletencab_recepcion_trans_inter
integer x = 3310
end type

event pb_grabar::clicked;Parent.TriggerEvent("ue_guardar")
//w_maed_recfruprocee_particular_trans.TriggerEvent("ue_guardar")
w_maed_recfruprocee_particular_trans.pb_grabar.PostEvent(Clicked!)
end event

type pb_imprimir from w_maed_palletencab_trans`pb_imprimir within w_maed_palletencab_recepcion_trans_inter
end type

type pb_salir from w_maed_palletencab_trans`pb_salir within w_maed_palletencab_recepcion_trans_inter
end type

event pb_salir::clicked;grabadocrel()
Close(Parent)

//Integer	li_respue
//
//IF istr_mant2.agrega = False AND istr_mant2.borra = False THEN
//	CloseWithReturn(Parent, istr_mant2)
//ELSE
//	IF dw_3.RowCount() < ii_CantPallets + ii_CantPuchos THEN
//		li_respue	=	MessageBox("ADVERTENCIA","No se ha completado la cantidad de Pallets~r" + &
//							"indicados en la Recepción.~r~rDesea retornar a ventana de recepción.", &
//							Question!, YesNo!)
//			IF li_respue = 1 THEN
//				CloseWithReturn(Parent, istr_mant2)
//			ELSE
//				pb_nuevo.SetFocus()
//			END IF
//	ELSE
//		CloseWithReturn(Parent, istr_mant2)
//	END IF
//END IF
//	
//
end event

type pb_ins_det from w_maed_palletencab_trans`pb_ins_det within w_maed_palletencab_recepcion_trans_inter
integer y = 1488
end type

type pb_eli_det from w_maed_palletencab_trans`pb_eli_det within w_maed_palletencab_recepcion_trans_inter
end type

type pb_buscar from w_maed_palletencab_trans`pb_buscar within w_maed_palletencab_recepcion_trans_inter
boolean visible = false
end type

type dw_histoencab from w_maed_palletencab_trans`dw_histoencab within w_maed_palletencab_recepcion_trans_inter
end type

type dw_histofruta from w_maed_palletencab_trans`dw_histofruta within w_maed_palletencab_recepcion_trans_inter
integer y = 1444
end type

type dw_3 from datawindow within w_maed_palletencab_recepcion_trans_inter
boolean visible = false
integer x = 27
integer y = 1596
integer width = 3378
integer height = 432
integer taborder = 110
boolean bringtotop = true
boolean titlebar = true
string dataobject = "dw_mues_recfruproced_trans"
boolean minbox = true
boolean maxbox = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
end type

type dw_4 from datawindow within w_maed_palletencab_recepcion_trans_inter
boolean visible = false
integer x = 297
integer y = 1852
integer width = 3159
integer height = 232
integer taborder = 100
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_inspecpaldet_informada_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_6 from datawindow within w_maed_palletencab_recepcion_trans_inter
boolean visible = false
integer x = 896
integer y = 1892
integer width = 1605
integer height = 368
integer taborder = 50
boolean bringtotop = true
string title = "Fechas Embalajes"
string dataobject = "dw_mues_palletfruta_fecemb"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_5 from datawindow within w_maed_palletencab_recepcion_trans_inter
boolean visible = false
integer x = 64
integer y = 1780
integer width = 2939
integer height = 232
integer taborder = 110
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_inspecpalenc_informada_trans"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_7 from datawindow within w_maed_palletencab_recepcion_trans_inter
boolean visible = false
integer x = 3118
integer y = 996
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_palletencab_2"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

