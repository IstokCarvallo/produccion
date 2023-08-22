$PBExportHeader$w_mant_mues_spro_camexisfg_directa.srw
$PBExportComments$Traspaso de Camara del Lote Fruta Granel Directo
forward
global type w_mant_mues_spro_camexisfg_directa from w_mant_directo
end type
type dw_5 from datawindow within w_mant_mues_spro_camexisfg_directa
end type
type dw_2 from datawindow within w_mant_mues_spro_camexisfg_directa
end type
type dw_3 from datawindow within w_mant_mues_spro_camexisfg_directa
end type
type dw_4 from datawindow within w_mant_mues_spro_camexisfg_directa
end type
end forward

global type w_mant_mues_spro_camexisfg_directa from w_mant_directo
integer width = 3593
string title = "Traslado de Fruta Granel en Camara"
event ue_aplica ( )
dw_5 dw_5
dw_2 dw_2
dw_3 dw_3
dw_4 dw_4
end type
global w_mant_mues_spro_camexisfg_directa w_mant_mues_spro_camexisfg_directa

type variables

DataWindowChild		idwc_planta, idwc_camaraorig, idwc_camaradest, idwc_envase, &
                     idwc_especie, idwc_tipoenva


uo_plantadesp			iuo_plantadesp
uo_plantadesp			iuo_plantalote
uo_camarasfrigo		iuo_camarasfrigoorig
uo_camarasfrigo		iuo_camarasfrigodest
uo_lotesfrutagranel	iuo_Lotes

Integer					ii_planta, ii_tipomovto
end variables

forward prototypes
public function boolean detlote (integer ai_planta, integer ai_camara, string as_lote)
public function boolean existetipoenva (integer ai_tipoen)
public function boolean existefolio (long al_folio)
public function boolean lotecondet (integer ai_planta, integer ai_camara, string as_lote, integer ai_tipoen, integer ai_envase)
public function integer existelote (integer ai_planta, integer ai_camara, string as_lote)
public function long duplicado (string planta, string tipoenva, string especie, string envase, string lote, datawindow adw_busca)
protected function boolean wf_actualiza_db ()
public subroutine habilitaencab (boolean habilita)
public function boolean lotesdestarados (string as_lote)
public subroutine actualizamovto ()
public subroutine borralote ()
public function boolean buscaduplicado (string as_columna, string as_valor)
public subroutine buscafolio ()
public subroutine buscalote ()
end prototypes

event ue_aplica();Long		ll_fila2, ll_fila3, ll_canbul, ll_tobul, ll_fila4
Integer	li_causal, li_NumeroLote, li_row
String	ls_NumeroLote, ls_lote

dw_2.AcceptText()

IF dw_2.RowCount() > 0 THEN
	FOR li_row = 1 TO dw_2.RowCount()
		 ls_Lote = dw_2.Object.lote[li_row]
		 IF Not lotesdestarados(ls_lote) AND dw_2.Object.tot_bultos[li_row] > 0 THEN
			 Messagebox("Error de Consistencia","El lote " + ls_lote + " no ha sido destarado" +&
			 												"~r~rPor favor ingrese otro lote")
			 Message.DoubleParm = -1
			 RETURN
		END IF
	NEXT
END IF

dw_1.Reset()
dw_4.Reset()

dw_1.Retrieve(Integer(istr_mant.Argumento[2]), Integer(istr_mant.Argumento[4]))
dw_4.Retrieve(Integer(istr_mant.Argumento[2]), Integer(istr_mant.Argumento[3]))

FOR ll_fila2 = 1 TO dw_2.Rowcount()
 IF  dw_2.Object.tot_bultos[ll_fila2] <= dw_2.Object.caex_canbul[ll_fila2] THEN
	IF dw_2.Object.tot_bultos[ll_fila2] > 0  THEN
		IF dw_1.Rowcount() > 0 THEN
				ll_fila3 = 	Duplicado(String(dw_2.Object.lote_pltcod[ll_fila2]), &
											 String(dw_2.Object.enva_tipoen[ll_fila2]), &
											 String(dw_2.Object.lote_espcod[ll_fila2]), &
											 String(dw_2.Object.enva_codigo[ll_fila2]), &
											 String(dw_2.Object.lote_codigo[ll_fila2]),dw_1)
				IF ll_fila3 = 0 THEN
					ll_fila3 = dw_1.InsertRow(0)
					dw_1.Object.lote_pltcod[ll_fila3] = dw_2.Object.lote_pltcod[ll_fila2]//gstr_ParamPlanta.CodigoPlanta
					dw_1.Object.lote_espcod[ll_fila3] = dw_2.Object.lote_espcod[ll_fila2]
					dw_1.Object.lote_codigo[ll_fila3] = dw_2.Object.lote_codigo[ll_fila2]
					dw_1.Object.plde_codigo[ll_fila3] = dw_2.Object.plde_codigo[ll_fila2]
					dw_1.Object.cama_codigo[ll_fila3] = Integer(Istr_Mant.Argumento[4])
					dw_1.Object.enva_tipoen[ll_fila3] = dw_2.Object.enva_tipoen[ll_fila2]
					dw_1.Object.enva_codigo[ll_fila3] = dw_2.Object.enva_codigo[ll_fila2]
					dw_1.Object.enva_nombre[ll_fila3] = dw_2.Object.enva_nombre[ll_fila2]
					dw_1.Object.tien_nombre[ll_fila3] = dw_2.Object.tien_nombre[ll_fila2]
					
					dw_1.Object.caex_nroban[ll_fila3] = dw_2.Object.caex_nroban[ll_fila2]
					dw_1.Object.caex_nropos[ll_fila3] = dw_2.Object.caex_nropos[ll_fila2]
					dw_1.Object.caex_nropis[ll_fila3] = dw_2.Object.caex_nropis[ll_fila2]
					
					dw_1.Object.caex_canbul[ll_fila3] = Long(dw_2.Object.tot_bultos[ll_fila2])
					dw_1.Object.tot_bultos[ll_fila3]	= dw_2.Object.tot_bultos[ll_fila2]

					/*Actualiza camara origen */
					ll_fila4 = 	Duplicado(String(dw_2.Object.lote_pltcod[ll_fila2]), &
											    String(dw_2.Object.enva_tipoen[ll_fila2]), &
											    String(dw_2.Object.lote_espcod[ll_fila2]), &
											    String(dw_2.Object.enva_codigo[ll_fila2]), &
											    String(dw_2.Object.lote_codigo[ll_fila2]),dw_4)
					IF ll_FIla4 > 0 THEN
						dw_4.Object.caex_canbul[ll_fila4] =	Long(dw_2.Object.caex_canbul[ll_fila2])-Long(dw_2.Object.tot_bultos[ll_fila2])
					END IF	
					
					/*Rebaje de Cajas*/
					
					dw_2.Object.caex_canbul[ll_fila2] =	Long(dw_2.Object.caex_canbul[ll_fila2])-Long(dw_2.Object.tot_bultos[ll_fila2])
					dw_2.Object.tot_bultos[ll_fila2]	 =	0
					
					dw_2.SetItemStatus(ll_fila2, 0,  Primary!, Datamodified!)
					dw_1.SetItemStatus(ll_fila3, 0,  Primary!, Newmodified!)
					
				ELSE
		
					ll_canbul	= Long(dw_1.Object.caex_canbul[ll_fila3])
					ll_tobul		= Long(dw_2.Object.tot_bultos[ll_fila2])
					dw_1.Object.tot_bultos[ll_fila3]	= ll_tobul
					dw_1.Object.caex_canbul[ll_fila3] =ll_canbul +ll_tobul
					
					/*Actualiza camara origen */
					ll_fila4 = 	Duplicado(String(dw_2.Object.lote_pltcod[ll_fila2]), &
											    String(dw_2.Object.enva_tipoen[ll_fila2]), &
											    String(dw_2.Object.lote_espcod[ll_fila2]), &
											    String(dw_2.Object.enva_codigo[ll_fila2]), &
											    String(dw_2.Object.lote_codigo[ll_fila2]),dw_4)
					IF ll_FIla4 > 0 THEN
						dw_4.Object.caex_canbul[ll_fila4] =	Long(dw_2.Object.caex_canbul[ll_fila2])-Long(dw_2.Object.tot_bultos[ll_fila2])
					END IF	
					
					dw_2.Object.caex_canbul[ll_fila2] =	Long(dw_2.Object.caex_canbul[ll_fila2])-Long(dw_2.Object.tot_bultos[ll_fila2])
					dw_2.Object.tot_bultos[ll_fila2]	 =	0
	
					dw_1.SetItemStatus(ll_fila3, 0,  Primary!, Datamodified!)
					dw_2.SetItemStatus(ll_fila2, 0,  Primary!, Datamodified!)
		
				END IF
		ELSE
			
			ll_fila3 = dw_1.InsertRow(0)
			dw_1.Object.lote_pltcod[ll_fila3] = dw_2.Object.lote_pltcod[ll_fila2]
			dw_1.Object.lote_espcod[ll_fila3] = dw_2.Object.lote_espcod[ll_fila2]
			dw_1.Object.lote_codigo[ll_fila3] = dw_2.Object.lote_codigo[ll_fila2]
			dw_1.Object.plde_codigo[ll_fila3] = dw_2.Object.plde_codigo[ll_fila2]
			dw_1.Object.cama_codigo[ll_fila3] = Integer(Istr_Mant.Argumento[4])
			dw_1.Object.enva_tipoen[ll_fila3] = dw_2.Object.enva_tipoen[ll_fila2]
			dw_1.Object.enva_codigo[ll_fila3] = dw_2.Object.enva_codigo[ll_fila2]
			dw_1.Object.enva_nombre[ll_fila3] = dw_2.Object.enva_nombre[ll_fila2]
			dw_1.Object.tien_nombre[ll_fila3] = dw_2.Object.tien_nombre[ll_fila2]
			dw_1.Object.caex_nroban[ll_fila3] = dw_2.Object.caex_nroban[ll_fila2]
			dw_1.Object.caex_nropos[ll_fila3] = dw_2.Object.caex_nropos[ll_fila2]
			dw_1.Object.caex_nropis[ll_fila3] = dw_2.Object.caex_nropis[ll_fila2]
			dw_1.Object.caex_canbul[ll_fila3] = Long(dw_2.Object.tot_bultos[ll_fila2])
			dw_1.Object.tot_bultos[ll_fila3]	= dw_2.Object.tot_bultos[ll_fila2]
			
			/*Actualiza camara origen */
			ll_fila4 = 	Duplicado(String(dw_2.Object.lote_pltcod[ll_fila2]), &
									    String(dw_2.Object.enva_tipoen[ll_fila2]), &
									    String(dw_2.Object.lote_espcod[ll_fila2]), &
									    String(dw_2.Object.enva_codigo[ll_fila2]), &
									    String(dw_2.Object.lote_codigo[ll_fila2]),dw_4)
			IF ll_FIla4 > 0 THEN
				dw_4.Object.caex_canbul[ll_fila4] =	Long(dw_2.Object.caex_canbul[ll_fila2])-Long(dw_2.Object.tot_bultos[ll_fila2])
			END IF	
			
			/*Rebaje de Cajas*/
			dw_2.Object.caex_canbul[ll_fila2] =	Long(dw_2.Object.caex_canbul[ll_fila2])-Long(dw_2.Object.tot_bultos[ll_fila2])
			dw_2.Object.tot_bultos[ll_fila2]	 =	0
						
			dw_2.SetItemStatus(ll_fila2, 0,  Primary!, Datamodified!)
			dw_1.SetItemStatus(ll_fila3, 0,  Primary!, Newmodified!)	
			
		END IF
	ELSE
		messagebox("Atención","La Fila " + string(ll_fila2) + " debe tener Bultos a Traspasar mayores a Cero.")
		dw_2.SetItemStatus(ll_fila2, 0,  Primary!, Notmodified!)
		message.doubleparm = -1
		RETURN 
	END IF	
 ELSE
	messagebox("Atención","La Fila " + string(ll_fila2) + " debe tener Bultos a Traspasar menores a Bultos del Lote.")
	dw_2.SetItemStatus(ll_fila2, 0,  Primary!, Notmodified!)
	message.doubleparm = -1
	RETURN 
 END IF		
NEXT	

end event

public function boolean detlote (integer ai_planta, integer ai_camara, string as_lote);Integer	li_lotplt, li_especie, li_lote, li_tipoen, li_envase, &
			li_banda, li_posicion, li_piso, li_bultos
Long 		ll_procod
String   ls_nomtipoen, ls_nomenvase,ls_pronom, ls_varnom

li_lotplt 	= Integer(Mid(as_lote,1,4))
li_especie 	= Integer(Mid(as_lote,5,2))
li_lote		= Integer(Mid(as_lote,7,4))


Select cfg.enva_tipoen, cfg.enva_codigo, cfg.caex_nroban, 
		 cfg.caex_nropos, cfg.caex_nropis, cfg.caex_canbul,
		 ten.tien_nombre, env.enva_nombre, pro.prod_codigo,
		 pro.prod_nombre, vrd.vari_nombre
Into	 :li_tipoen, :li_envase, :li_banda, :li_posicion, 
		 :li_piso, :li_bultos, :ls_nomtipoen, :ls_nomenvase,
		 :ll_procod, :ls_pronom, :ls_varnom
From   dba.spro_camaraexistefg cfg, dba.tiposenvases ten, dba.envases env,
		 dba.productores pro, dba.variedades vrd, dba.spro_lotesfrutagranel lfg
Where  cfg.plde_codigo =: ai_planta
And    cfg.cama_codigo =: ai_camara
And    cfg.lote_pltcod =: li_lotplt
AND    cfg.lote_espcod =: li_especie
And    cfg.lote_codigo =: li_lote
And    lfg.lote_pltcod =  cfg.lote_pltcod
And    lfg.lote_espcod =  cfg.lote_espcod
And    lfg.lote_codigo =  cfg.lote_codigo
And    lfg.prod_codigo =  pro.prod_codigo
And    lfg.lote_espcod =  vrd.espe_codigo
And    lfg.vari_codigo =  vrd.vari_codigo
And    cfg.enva_tipoen =  ten.enva_tipoen
And    cfg.enva_tipoen =  env.enva_tipoen
And    cfg.enva_codigo =  env.enva_codigo;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de spro_camaraexistefg")
		
		RETURN FALSE 
	ELSEIF SQLCA.SQLCode = 100 THEN
		MessageBox("Atención","Número de lote No Existe")
		RETURN FALSE
	ELSE
		dw_2.Object.plde_codigo[il_fila] = ai_planta
		dw_2.Object.cama_codigo[il_fila]	= ai_camara
		dw_2.Object.lote_pltcod[il_fila]	= li_lotplt
		dw_2.Object.lote_espcod[il_fila]	= li_especie
		dw_2.Object.lote_codigo[il_fila]	= li_lote
		dw_2.Object.prod_codigo[il_fila]	= ll_procod
		dw_2.Object.prod_nombre[il_fila]	= ls_pronom
		dw_2.Object.vari_nombre[il_fila]	= ls_varnom
		dw_2.Object.enva_tipoen[il_fila]	= li_tipoen
		dw_2.GetChild("enva_codigo",idwc_envase)
		idwc_envase.SetTransObject(SQLCA)
		IF idwc_envase.Retrieve(li_tipoen)=0 THEN
			idwc_envase.InsertRow(0)
		END IF
		dw_2.Object.enva_codigo[il_fila]	= li_envase
		dw_2.Object.caex_nroban[il_fila]	= li_banda
		dw_2.Object.caex_nropos[il_fila]	= li_posicion
		dw_2.Object.caex_nropis[il_fila]	= li_piso	
		dw_2.Object.caex_canbul[il_fila]	= li_bultos
//		dw_2.Object.tien_nombre[il_fila] = ls_nomtipoen
		dw_2.Object.enva_nombre[il_fila] = ls_nomenvase
		dw_2.Object.lote[il_fila]	=  String(li_lotplt,'0000') + &
												String(li_especie,'00') + &
												String(li_lote,'0000')
		pb_grabar.Enabled = True
		RETURN TRUE				
		
END IF
end function

public function boolean existetipoenva (integer ai_tipoen);Integer il_codigo

SELECT enva_tipoen INTO :il_codigo
  FROM dba.tiposenvases
 WHERE enva_tipoen = :ai_tipoen
   AND tien_usoenv = 1;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Tipos de Envases" )
	RETURN FALSE	
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","El Código de Tipo de Envase no Existe.")
	RETURN FALSE
END IF

RETURN TRUE
end function

public function boolean existefolio (long al_folio);Long ll_Folio
Integer li_planta

li_planta = gstr_paramplanta.codigoplanta

SELECT mvce_numero INTO :ll_Folio
  FROM dba.spro_movtocamarafgenca
 WHERE plde_codigo = :li_planta
   AND tpmv_codigo = 81
   AND mvce_numero = :al_folio;
		 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Movimiento de Camara Encabezado" )
	RETURN FALSE	
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","El Número de Folio no Existe. Ingrese Otro Folio.")
	RETURN FALSE
END IF
		 
RETURN TRUE		 
end function

public function boolean lotecondet (integer ai_planta, integer ai_camara, string as_lote, integer ai_tipoen, integer ai_envase);Integer	li_lotplt, li_especie, li_lote, li_tipoen, li_envase, &
			li_banda, li_posicion, li_piso, li_bultos
Long		ll_procod
String   ls_nomtipoen, ls_nomenvase,ls_pronom, ls_varnom, ls_null

SetNull(ls_Null)

li_lotplt 	= Integer(Mid(as_lote,1,4))
li_especie 	= Integer(Mid(as_lote,5,2))
li_lote		= Integer(Mid(as_lote,7,4))


Select cfg.enva_tipoen, cfg.enva_codigo, cfg.caex_nroban, 
		 cfg.caex_nropos, cfg.caex_nropis, cfg.caex_canbul,
		 ten.tien_nombre, env.enva_nombre, pro.prod_codigo,
		 pro.prod_nombre, vrd.vari_nombre
Into	 :li_tipoen, :li_envase, :li_banda, :li_posicion, 
		 :li_piso, :li_bultos, :ls_nomtipoen, :ls_nomenvase,
		 :ll_procod, :ls_pronom, :ls_varnom
From   dba.spro_camaraexistefg cfg, dba.tiposenvases ten, dba.envases env,
		 dba.productores pro, dba.variedades vrd, dba.spro_lotesfrutagranel lfg
Where  cfg.plde_codigo =: ai_planta
And    cfg.cama_codigo =: ai_camara
And    cfg.lote_pltcod =: li_lotplt
And    cfg.lote_espcod =: li_especie
And    cfg.lote_codigo =: li_lote
And	 cfg.enva_tipoen =: ai_tipoen
And    cfg.enva_codigo =: ai_envase
And    lfg.lote_pltcod =  cfg.lote_pltcod
And    lfg.lote_espcod =  cfg.lote_espcod
And    lfg.lote_codigo =  cfg.lote_codigo
And    lfg.prod_codigo =  pro.prod_codigo
And    lfg.lote_espcod =  vrd.espe_codigo
And    lfg.vari_codigo =  vrd.vari_codigo
And    cfg.enva_tipoen =  ten.enva_tipoen
And    cfg.enva_tipoen =  env.enva_tipoen
And    cfg.enva_codigo =  env.enva_codigo;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de spro_camaraexistefg")
		
		RETURN FALSE 
	ELSEIF SQLCA.SQLCode = 100 THEN
		MessageBox("Atención","Envases no Corresponden al Lote")
		dw_2.SetItem(il_fila,"enva_codigo",integer(ls_Null))
		dw_2.SetItem(il_fila,"enva_nombre",integer(ls_Null))
		dw_2.SetItem(il_fila,"caex_canbul",integer(ls_Null))
		RETURN FALSE
	ELSE
		dw_2.Object.plde_codigo[il_fila] = ai_planta
		dw_2.Object.cama_codigo[il_fila]	= ai_camara
		dw_2.Object.lote_pltcod[il_fila]	= li_lotplt
		dw_2.Object.lote_espcod[il_fila]	= li_especie
		dw_2.Object.lote_codigo[il_fila]	= li_lote
		dw_2.Object.prod_codigo[il_fila]	= ll_procod
		dw_2.Object.prod_nombre[il_fila]	= ls_pronom
		dw_2.Object.vari_nombre[il_fila]	= ls_varnom
		dw_2.Object.enva_tipoen[il_fila]	= li_tipoen
		dw_2.Object.enva_codigo[il_fila]	= li_envase
		dw_2.Object.caex_nroban[il_fila]	= li_banda
		dw_2.Object.caex_nropos[il_fila]	= li_posicion
		dw_2.Object.caex_nropis[il_fila]	= li_piso	
		dw_2.Object.caex_canbul[il_fila]	= li_bultos
 	   dw_2.Object.tien_nombre[il_fila] = ls_nomtipoen
		dw_2.Object.enva_nombre[il_fila] = ls_nomenvase
		dw_2.Object.lote[il_fila]	=  String(li_lotplt,'0000') + &
												String(li_especie,'00') + &
												String(li_lote,'0000')
		pb_grabar.Enabled = True
		RETURN TRUE				
END IF

end function

public function integer existelote (integer ai_planta, integer ai_camara, string as_lote);Integer	li_lotplt, li_especie, li_lote, li_Cant
String   ls_nomtipoen, ls_nomenvase

li_lotplt 	= Integer(Mid(as_lote,1,4))
li_especie 	= Integer(Mid(as_lote,5,2))
li_lote		= Integer(Mid(as_lote,7,4))


Select Count(*)
Into	 :li_Cant
From   dba.spro_camaraexistefg cfg
Where  cfg.plde_codigo =: ai_planta
And    cfg.cama_codigo =: ai_camara
And    cfg.lote_pltcod =: li_lotplt
AND    cfg.lote_espcod =: li_especie
And    cfg.lote_codigo =: li_lote;

	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Tabla de spro_camaraexistefg")
		
		RETURN li_Cant 
	ELSEIF li_Cant = 0 THEN
		MessageBox("Atención","Número de lote No Existe")
		RETURN li_Cant
	ELSE
		RETURN li_Cant
		
END IF
end function

public function long duplicado (string planta, string tipoenva, string especie, string envase, string lote, datawindow adw_busca);Long		ll_Fila
String	ls_codigo

ll_fila	= adw_busca.Find("lote_pltcod =" + planta + " AND " + "enva_tipoen =" + TipoEnva + " AND " + &
							"lote_espcod =" + especie+ " AND " + "enva_codigo =" + Envase   + " AND " + &
							"lote_codigo = " + lote, 1, adw_busca.RowCount())

RETURN ll_Fila



end function

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

dw_1.Accepttext()
dw_4.Accepttext()

IF Not dw_1.uf_check_required(0) THEN RETURN False

IF Not dw_1.uf_validate(0) THEN RETURN False

IF dw_1.Update(True, False) = 1 THEN 
	IF dw_4.Update(True, False) = 1 THEN 
		IF dw_5.Update(True, False) = 1 THEN 
			IF dw_3.Update(True, False) = 1 THEN
				Commit;

				IF sqlca.SQLCode <> 0 THEN
					F_ErrorBaseDatos(sqlca, This.Title)
					lb_Retorno	=	False
				ELSE
					lb_Retorno	=	True
						
					dw_1.ResetUpdate()
					dw_4.ResetUpdate()
					dw_5.ResetUpdate()
					dw_3.ResetUpdate()
					
				END IF
			ELSE
				RollBack;
			
				IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
				
				lb_Retorno	=	False
			END IF
		ELSE
			RollBack;
			
			IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
			
			lb_Retorno	=	False
		END IF
	ELSE
		RollBack;
		
		IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
		
		lb_Retorno	=	False
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	  =	lb_AutoCommit
pb_eliminar.enabled =	False
RETURN lb_Retorno


end function

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_5.Object.plde_codigo.Protect = 0
	dw_5.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_5.Object.mvce_numero.Protect = 0
	dw_5.Object.mvce_numero.BackGround.Color	=	RGB(255,255,255)
	dw_5.Object.mvce_fecmov.Protect = 0
	dw_5.Object.mvce_fecmov.BackGround.Color	=	RGB(255,255,255)
	dw_5.Object.espe_codigo.Protect = 0
	dw_5.Object.espe_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_5.Object.cama_codori.Protect = 0
	dw_5.Object.cama_codori.BackGround.Color	=	RGB(255,255,255)
	dw_5.Object.cama_coddes.Protect = 0
	dw_5.Object.cama_coddes.BackGround.Color	=	RGB(255,255,255)
	
	dw_5.Object.b_folio.visible					=  1
	
ELSE
	dw_5.Object.plde_codigo.Protect = 1
	dw_5.Object.plde_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_5.Object.mvce_numero.Protect = 1
	dw_5.Object.mvce_numero.BackGround.Color	=	RGB(192,192,192)
	dw_5.Object.mvce_fecmov.Protect = 1
	dw_5.Object.mvce_fecmov.BackGround.Color	=	RGB(192,192,192)
	dw_5.Object.espe_codigo.Protect = 1
	dw_5.Object.espe_codigo.BackGround.Color	=	RGB(192,192,192)
	dw_5.Object.cama_codori.Protect = 1
	dw_5.Object.cama_codori.BackGround.Color	=	RGB(192,192,192)
	dw_5.Object.cama_coddes.Protect = 1
	dw_5.Object.cama_coddes.BackGround.Color	=	RGB(192,192,192)
	
	dw_5.Object.b_folio.visible					=  0
	
END IF
end subroutine

public function boolean lotesdestarados (string as_lote);Integer li_codigo, li_plt, li_esp, li_lot
String  ls_nombre
Boolean lb_Retorno=True

li_plt = Integer(Mid(as_lote,1,4))
li_esp = Integer(Mid(as_lote,5,2))
li_lot = Integer(Mid(as_lote,7,4))

SELECT lote_codigo
  INTO :li_codigo
  FROM dba.spro_lotesfrutagranel
 WHERE lote_pltcod = :li_plt
	AND lote_espcod = :li_esp
	AND lote_codigo = :li_lot
	AND lote_totnet > 0;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla de Lotes Fruta Granel")
	lb_Retorno	=	False
ELSEIF sqlca.SQLCode = 100 THEN
	lb_Retorno	=	False
END IF	
RETURN lb_retorno

end function

public subroutine actualizamovto ();Long ll_fila2, ll_FilaNueva , ll_fila1, ll_fila, ll_tobul, ll_filamov
string ls_lotepl, ls_lotees, ls_loteco, ls_tipoen, ls_envase

 FOR ll_fila1 = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila1, 0, Primary!) = NewModified! OR &
		dw_1.GetItemStatus(ll_fila1, 0, Primary!) = DataModified!THEN
	
	      ls_lotepl = string(dw_1.Object.lote_pltcod[ll_fila1])
			ls_lotees = string(dw_1.Object.lote_espcod[ll_fila1])
			ls_loteco = string(dw_1.Object.lote_codigo[ll_fila1])
			ls_tipoen = string(dw_1.Object.enva_tipoen[ll_fila1])
			ls_envase = string(dw_1.Object.enva_codigo[ll_fila1])
			
			ll_filaMov	= dw_3.Find("lote_pltcod =" + ls_lotepl + " AND enva_tipoen =" + ls_tipoen + " AND " + &
										   "lote_espcod =" + ls_lotees + " AND enva_codigo =" + ls_envase + " AND " + &
										   "lote_codigo =" + ls_loteco + " AND mvca_tipomv = 1", 1, dw_3.RowCount())

			IF ll_FilaMov = 0 THEN
				ll_FilaNueva = dw_3.InsertRow(0)
				dw_3.Object.lote_pltcod[ll_FilaNueva]  = dw_1.Object.lote_pltcod[ll_fila1]
				dw_3.Object.lote_espcod[ll_FilaNueva]  = dw_1.Object.lote_espcod[ll_fila1]
				dw_3.Object.lote_codigo[ll_FilaNueva]  = dw_1.Object.lote_codigo[ll_fila1]
				dw_3.Object.mvca_tipomv[ll_FilaNueva]	= 1
				dw_3.Object.mvca_canbul[ll_FilaNueva]	= dw_1.Object.tot_bultos[ll_fila1] 
				dw_3.Object.cama_codigo[ll_FilaNueva]	= Integer(istr_Mant.Argumento[4])
				dw_3.Object.mvca_nroban[ll_FilaNueva]	= dw_1.Object.caex_nroban[ll_fila1]
				dw_3.Object.mvca_nropos[ll_FilaNueva]	= dw_1.Object.caex_nropos[ll_fila1]
				dw_3.Object.mvca_nropis[ll_FilaNueva]	= dw_1.Object.caex_nropis[ll_fila1]			
				dw_3.Object.enva_tipoen[ll_FilaNueva]  = dw_1.Object.enva_tipoen[ll_fila1]
				dw_3.Object.enva_codigo[ll_FilaNueva]  = dw_1.Object.enva_codigo[ll_fila1]
				dw_3.Object.enva_nombre[ll_FilaNueva]  = dw_1.Object.enva_nombre[ll_fila1]
				dw_3.Object.tien_nombre[ll_FilaNueva]  = dw_1.Object.tien_nombre[ll_fila1]
				ll_tobul = dw_1.Object.tot_bultos[ll_fila1]
				dw_3.Object.mvca_canbul[ll_FilaNueva]	= ll_tobul
				dw_3.SetItemStatus(ll_filaNueva, 0,  Primary!, NewModified!)
			ELSEIF ll_FilaMov>0 THEN
				dw_3.Object.mvca_nroban[ll_FilaMov]	= dw_1.Object.caex_nroban[ll_fila1]
				dw_3.Object.mvca_nropos[ll_FilaMov]	= dw_1.Object.caex_nropos[ll_fila1]
				dw_3.Object.mvca_nropis[ll_FilaMov]	= dw_1.Object.caex_nropis[ll_fila1]			
				ll_tobul = dw_1.Object.tot_bultos[ll_fila1]+dw_3.Object.mvca_canbul[ll_FilaMov]
				dw_3.Object.mvca_canbul[ll_FilaMov]	= ll_tobul
			END IF	
	END IF
NEXT

FOR ll_fila2 = 1 TO dw_2.RowCount()
	IF dw_2.GetItemStatus(ll_fila2, 0, Primary!) = DataModified! THEN
		
      ls_lotepl = string(dw_2.Object.lote_pltcod[ll_fila2])
		ls_lotees = string(dw_2.Object.lote_espcod[ll_fila2])
		ls_loteco = string(dw_2.Object.lote_codigo[ll_fila2])
		ls_tipoen = string(dw_2.Object.enva_tipoen[ll_fila2])
		ls_envase = string(dw_2.Object.enva_codigo[ll_fila2])
		
		ll_filaMov	= dw_3.Find("lote_pltcod =" + ls_lotepl + " AND enva_tipoen =" + ls_tipoen + " AND " + &
		 								"lote_espcod =" + ls_lotees + " AND enva_codigo =" + ls_envase + " AND " + &
										"lote_codigo =" + ls_loteco + " AND mvca_tipomv = 2", 1, dw_3.RowCount())
		IF ll_FilaMov=0 THEN								
			ll_FilaNueva = dw_3.InsertRow(0)
			dw_3.Object.lote_pltcod[ll_FilaNueva]  = dw_2.Object.lote_pltcod[ll_fila2]
			dw_3.Object.lote_espcod[ll_FilaNueva]  = dw_2.Object.lote_espcod[ll_fila2]
			dw_3.Object.lote_codigo[ll_FilaNueva]  = dw_2.Object.lote_codigo[ll_fila2]
			dw_3.Object.mvca_tipomv[ll_FilaNueva]	= 2
			dw_3.Object.cama_codigo[ll_FilaNueva]	= Integer(istr_Mant.Argumento[3])
			dw_3.Object.mvca_nroban[ll_FilaNueva]	= dw_2.Object.caex_nroban[ll_fila2]
			dw_3.Object.mvca_nropos[ll_FilaNueva]	= dw_2.Object.caex_nropos[ll_fila2]
			dw_3.Object.mvca_nropis[ll_FilaNueva]	= dw_2.Object.caex_nropis[ll_fila2]
			ll_fila =  dw_1.Find("lote_pltcod =" + ls_lotepl + " AND enva_tipoen =" + ls_tipoen + " AND " + &
		 								"lote_espcod =" + ls_lotees + " AND enva_codigo =" + ls_envase + " AND " + &
										"lote_codigo =" + ls_loteco , 1, dw_1.RowCount())
			IF ll_fila>0 THEN							
			   dw_3.Object.mvca_canbul[ll_FilaNueva]	= dw_1.Object.tot_bultos[ll_fila]
			ELSE
				dw_3.Object.mvca_canbul[ll_FilaNueva]	= 0
			END IF	
			dw_3.Object.enva_tipoen[ll_FilaNueva]  = dw_2.Object.enva_tipoen[ll_fila2]
			dw_3.Object.enva_codigo[ll_FilaNueva]  = dw_2.Object.enva_codigo[ll_fila2]
			dw_3.Object.enva_nombre[ll_FilaNueva]  = dw_2.Object.enva_nombre[ll_fila2]
			dw_3.Object.tien_nombre[ll_FilaNueva]  = dw_2.Object.tien_nombre[ll_fila2]
			dw_3.SetItemStatus(ll_filaNueva, 0,  Primary!, NewModified!)
		ELSEIF ll_FilaMov>0 THEN
			dw_3.Object.mvca_nroban[ll_FilaMov]	= dw_1.Object.caex_nroban[ll_fila1]
			dw_3.Object.mvca_nropos[ll_FilaMov]	= dw_1.Object.caex_nropos[ll_fila1]
			dw_3.Object.mvca_nropis[ll_FilaMov]	= dw_1.Object.caex_nropis[ll_fila1]			
			ll_fila =  dw_1.Find("lote_pltcod =" + ls_lotepl + " AND enva_tipoen =" + ls_tipoen + " AND " + &
		 								"lote_espcod =" + ls_lotees + " AND enva_codigo =" + ls_envase + " AND " + &
										"lote_codigo =" + ls_loteco , 1, dw_1.RowCount())
			IF ll_fila>0 THEN							
				ll_tobul = dw_1.Object.tot_bultos[ll_fila]+dw_3.Object.mvca_canbul[ll_FilaMov]
			ELSE
				ll_tobul = dw_3.Object.mvca_canbul[ll_FilaMov]
			END IF	
			dw_3.Object.mvca_canbul[ll_FilaMov]	= ll_tobul			
		END IF	
	END IF
NEXT

end subroutine

public subroutine borralote ();Long ll_fila
ll_fila = 1

Do while ll_Fila <= dw_2.Rowcount()
	IF dw_2.Object.caex_canbul[ll_fila] = 0 THEN
		dw_2.DeleteRow(ll_fila)
	ELSE
		ll_Fila = ll_Fila + 1
	END IF
Loop

ll_fila = 1
Do while ll_Fila <= dw_4.Rowcount()
	IF dw_4.Object.caex_canbul[ll_fila] = 0 THEN
		dw_4.DeleteRow(ll_fila)
	ELSE
		ll_Fila = ll_Fila + 1
	END IF
Loop
end subroutine

public function boolean buscaduplicado (string as_columna, string as_valor);Long		ll_Fila
string  ls_lotepl, ls_lotees, ls_loteco, ls_tipoen, ls_envase

ls_lotepl = string(dw_2.Object.lote_pltcod[il_fila])
ls_lotees = string(dw_2.Object.lote_espcod[il_fila])
ls_loteco = string(dw_2.Object.lote_codigo[il_fila])
ls_tipoen = string(dw_2.Object.enva_tipoen[il_fila])
ls_envase = string(dw_2.Object.enva_codigo[il_fila])

CHOOSE CASE as_columna
	CASE "lote_pltcod"
		ls_lotepl = as_valor
	
	CASE "lote_espcod"
		ls_lotees = as_valor
		
	CASE "lote_codigo"
		ls_loteco = as_valor
		
	CASE "enva_tipoen"
		ls_tipoen = as_valor
		
	CASE "enva_codigo"
		ls_envase = as_valor
		
	END CHOOSE 
		
ll_fila	= dw_2.Find("lote_pltcod =" + ls_lotepl + " AND " + "enva_tipoen =" + ls_tipoen + " AND " + &
							"lote_espcod =" + ls_lotees + " AND " + "enva_codigo =" + ls_envase + " AND " + &
							"lote_codigo =" + ls_loteco, 1, dw_2.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Códigos de Lote, Ya Fueron ingresados anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

RETURN TRUE
end function

public subroutine buscafolio ();Str_busqueda	lstr_busq

lstr_busq.argum[1] = String(dw_5.Object.plde_codigo[1])
lstr_busq.argum[2] = String(dw_5.Object.mvce_fecmov[1])

OpenWithParm(w_busc_movtocamaenca, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[3] = "" THEN
	dw_5.SetColumn("mvce_numero")
	dw_5.SetFocus()
ELSE
	dw_5.Object.mvce_numero[1]	=	Long(lstr_busq.argum[3])
	istr_mant.argumento[6] 		=	lstr_busq.argum[3]
	triggerevent("ue_recuperadatos")

END IF

RETURN
end subroutine

public subroutine buscalote ();String  ls_Lote, ls_Null
Str_busqueda	lstr_busq

SetNull(ls_Null)

lstr_busq.argum[1] = string(dw_5.Object.plde_codigo[1])
lstr_busq.argum[2] = string(dw_5.Object.espe_codigo[1])
lstr_busq.argum[3] = istr_mant.Argumento[3]


OpenWithParm(w_busc_lotesfrutagranel_existencia, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[1] = "" THEN
	dw_2.SetColumn("lote")
	dw_2.SetFocus()
ELSE

	dw_2.Object.lote_espcod[il_fila] = Integer(lstr_busq.Argum[1])
	dw_2.Object.lote_pltcod[il_fila] = Integer(lstr_busq.Argum[2])
	dw_2.Object.lote_codigo[il_fila] = Integer(lstr_busq.Argum[3])

	ls_lote								=	String(dw_2.Object.lote_pltcod[il_fila],"0000") + &
												String(dw_2.Object.lote_espcod[il_fila],"00") + &
												String(dw_2.Object.lote_codigo[il_fila],"0000")

	dw_2.Object.lote[il_fila] = ls_lote
	dw_2.Object.plde_codigo[il_fila] = dw_5.Object.plde_codigo[1]
	dw_2.Object.vari_nombre[il_fila] = lstr_busq.Argum[5]
	dw_2.Object.prod_codigo[il_fila] = Long(lstr_busq.Argum[6])
	dw_2.Object.prod_nombre[il_fila] = lstr_busq.Argum[7]
	dw_2.Object.caex_canbul[il_fila] = Integer(lstr_busq.Argum[8])
	dw_2.Object.enva_tipoen[il_fila] = Integer(lstr_busq.Argum[10])
	dw_2.GetChild("enva_codigo",idwc_envase)
	idwc_envase.SetTransObject(SQLCA)
	IF idwc_envase.Retrieve(Integer(lstr_busq.Argum[10]))=0 THEN
		idwc_envase.InsertRow(0)
	END IF	
	dw_2.Object.enva_codigo[il_fila] = Integer(lstr_busq.Argum[11])
	dw_2.Object.enva_nombre[il_fila] = lstr_busq.Argum[12]
	
	pb_grabar.Enabled=True
	dw_2.SetColumn("caex_nroban")
	dw_2.SetFocus()
END IF

RETURN

end subroutine

on w_mant_mues_spro_camexisfg_directa.create
int iCurrent
call super::create
this.dw_5=create dw_5
this.dw_2=create dw_2
this.dw_3=create dw_3
this.dw_4=create dw_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_5
this.Control[iCurrent+2]=this.dw_2
this.Control[iCurrent+3]=this.dw_3
this.Control[iCurrent+4]=this.dw_4
end on

on w_mant_mues_spro_camexisfg_directa.destroy
call super::destroy
destroy(this.dw_5)
destroy(this.dw_2)
destroy(this.dw_3)
destroy(this.dw_4)
end on

event ue_imprimir();SetPointer(HourGlass!)

Integer	li_Planta, li_Tipmov
Long		Fila, ll_folio

str_info	lstr_info

lstr_info.titulo	= "INFORME TRASLADO DE CAMARAS"
lstr_info.copias	= 1

OpenWithParm(vinf, lstr_info)

vinf.dw_1.DataObject = "dw_info_spro_movtocamarafgenca"
vinf.dw_1.SetTransObject(sqlca)

li_Planta		=	dw_5.Object.plde_codigo[1]
li_tipmov      = 81
ll_Folio			=	dw_5.Object.mvce_numero[1]

Fila = vinf.dw_1.Retrieve(li_Planta, li_Tipmov, ll_Folio)

IF Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE	
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event ue_guardar();IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!) 

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN Return

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event open;/*
Argumentos
*********************************************
istr_Mant.Argumento[1]	=> Tipo de Movimiento
istr_Mant.Argumento[2]	=> Código de Planta
istr_Mant.Argumento[3]	=> Código de Camara Origen
istr_Mant.Argumento[4]	=> Código de Camara Destino.
*********************************************
*/

x				= 0
y				= 0

This.Height	= 1993
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono

//Planta
dw_5.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_planta.insertRow(0)
END IF

//Especie
dw_5.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(SqlCa)
IF idwc_especie.Retrieve(gstr_parempresa.empr_codexp) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
	idwc_especie.insertRow(0)
END IF

//Camara Origen
dw_5.GetChild("cama_codori", idwc_camaraorig)
idwc_camaraorig.SetTransObject(SqlCa)
IF idwc_camaraorig.Retrieve(gstr_paramplanta.codigoplanta) = 0 THEN
	MessageBox("Atención","Falta Registrar Camaras")
	idwc_camaraorig.InsertRow(0)
END IF

//Camara Destino
dw_5.GetChild("cama_coddes", idwc_camaradest)
idwc_camaradest.SetTransObject(SqlCa)
IF idwc_camaradest.Retrieve(gstr_paramplanta.codigoplanta) = 0 THEN
	MessageBox("Atención","Falta Registrar Camaras")
	idwc_camaradest.InsertRow(0)
END IF

dw_2.SetTransObject(SqlCa)
dw_3.SetTransObject(SqlCa)
dw_4.SetTransObject(SqlCa)
dw_5.SetTransObject(SqlCa)

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

buscar	= "Lote:Nlote_codigo,Banda:Ncaex_nroban,Columna:Ncaex_nropos,Altura:Ncaex_nropis"
ordenar	= "Lote:lote_codigo,Banda:caex_nroban,Columna:caex_nropos,Altura:caex_nropis"

istr_Mant.Argumento[1]	=	Message.StringParm
istr_Mant.Argumento[2]	=	String(gstr_ParamPlanta.CodigoPlanta)

istr_Mant.Argumento[6]	=	""

//Tipo Envase
dw_2.GetChild("enva_tipoen",idwc_tipoenva)
idwc_tipoenva.SetTransObject(SQLCA)
IF idwc_tipoenva.ReTrieve() = 0 THEN
	idwc_tipoenva.InsertRow(0)
ELSE
	idwc_tipoenva.SetFilter("tien_usoenv=1")
	idwc_tipoenva.Filter()
END IF 	

//Codigo del Envase
dw_2.GetChild("enva_codigo", idwc_envase)
idwc_envase.SetTransObject(SqlCa)
IF idwc_envase.Retrieve(0) = 0 THEN
	idwc_envase.InsertRow(0)
END IF

iuo_plantadesp			=	Create uo_plantadesp
iuo_plantalote       =  Create uo_plantadesp
iuo_camarasfrigoorig	=	Create uo_camarasfrigo
iuo_camarasfrigodest	=	Create uo_camarasfrigo
iuo_Lotes				=	Create uo_lotesfrutagranel

dw_5.InsertRow(0)
dw_5.SetItem(1, "plde_codigo", gstr_ParamPlanta.CodigoPlanta)
dw_5.SetItem(1, "mvce_fecmov",Today())
dw_5.SetItem(1, "espe_codigo", gstr_ParamPlanta.Codigoespecie)

dw_5.SetColumn("mvce_numero")
dw_5.SetFocus()

end event

event resize;Integer		li_posi_y, li_objeto

//gb_1.x 					= This.WorkSpaceWidth() - 351
//gb_1.y 					= 33
//gb_1.width				= 275
//gb_1.height				= 180 * 1 + 97 /*  (1 Botón)  */

pb_lectura.x			= This.WorkSpaceWidth() - 292
//pb_lectura.y			= gb_1.y + 88
pb_lectura.width		= 156
pb_lectura.height		= 133

//gb_2.x 					= This.WorkSpaceWidth() - 351
//gb_2.width				= 275

pb_nuevo.x				= This.WorkSpaceWidth() - 292
pb_nuevo.width			= 156
pb_nuevo.height		= 133

pb_insertar.x			= This.WorkSpaceWidth() - 292
pb_insertar.width		= 156
pb_insertar.height	= 133

pb_eliminar.x			= This.WorkSpaceWidth() - 292
pb_eliminar.width		= 156
pb_eliminar.height	= 133

pb_grabar.x				= This.WorkSpaceWidth() - 292
pb_grabar.width		= 156
pb_grabar.height		= 133

pb_imprimir.x			= This.WorkSpaceWidth() - 292
pb_imprimir.width		= 156
pb_imprimir.height	= 133


//IF st_encabe.Visible THEN
//	gb_2.y 					= dw_1.y - 36
//ELSE
//	gb_2.y 					= gb_1.y + gb_1.height + 30
//END IF

//li_posi_y	= gb_2.y - 92

IF pb_nuevo.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_nuevo.y	= li_posi_y
END IF

IF pb_insertar.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_insertar.y	= li_posi_y
END IF

IF pb_eliminar.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_eliminar.y	= li_posi_y
END IF

IF pb_grabar.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_grabar.y		= li_posi_y
END IF

IF pb_imprimir.Visible THEN
	li_objeto	++
	li_posi_y	+= 180
	pb_imprimir.y	= li_posi_y
END IF

//gb_2.height				= 180 * li_objeto + 97
//gb_3.x 					= This.WorkSpaceWidth() - 351
//gb_3.y 					= This.WorkSpaceHeight() - 345
//gb_3.width				= 275
//gb_3.height				= 180 * 1 + 97 /*  (1 Botón)  */

pb_salir.x				= This.WorkSpaceWidth() - 292
//pb_salir.y				= gb_3.y + 88
pb_salir.width			= 156
pb_salir.height		= 133
end event

event ue_antesguardar();Long ll_numero, ll_fila
Integer li_planta, li_tipmov

message.doubleparm = 1

TriggerEvent("ue_validaregistro")
IF	message.doubleparm = -1 THEN RETURN 


TriggerEvent("ue_aplica")
IF	message.doubleparm = -1 THEN RETURN 

Actualizamovto()

borralote()

li_planta = dw_5.Object.plde_codigo[1]
li_tipmov = 81

IF dw_5.GetItemStatus(1,0,Primary!) = NewModified! THEN
	
   UPDATE	dba.spro_movtocamarafgenca
		SET	mvce_numero = 0
		WHERE	1 = 2;
	
	SELECT	IsNull(Max(mvce_numero), 0) + 1
		INTO	:ll_Numero
		FROM	dba.spro_movtocamarafgenca
  	  WHERE	plde_codigo	=	:li_Planta
		AND	tpmv_codigo	=	:li_tipmov;
		
   dw_5.Object.plde_codigo[1] = li_planta
	dw_5.Object.tpmv_codigo[1] = li_tipmov
	dw_5.Object.mvce_numero[1] = ll_numero

ELSE
	
	ll_numero = dw_5.Object.mvce_numero[1]
	
END IF

FOR ll_fila=1 TO dw_3.RowCount()
	IF dw_3.GetItemStatus(ll_fila,0,Primary!) = NewModified! THEN
		dw_3.Object.plde_codigo[ll_fila] = li_planta
   	dw_3.Object.tpmv_codigo[ll_fila] = li_tipmov
	   dw_3.Object.mvce_numero[ll_fila] = ll_numero
	END IF	
NEXT 	
end event

event ue_nuevo();IF dw_5.Object.espe_codigo[1] = 0 OR isnull(dw_5.Object.espe_codigo[1]) THEN
	MessageBox("Error","Debe Seleccionar una Especie.")	
   RETURN
END IF	

IF istr_mant.argumento[3] = "" THEN
   MessageBox("Error","Debe Seleccionar una Camara Origen.")	
   RETURN
END IF

IF istr_mant.argumento[4] = "" THEN
   MessageBox("Error","Debe Seleccionar una Camara Destino.")	
   RETURN
END IF

IF (istr_Mant.Argumento[3] <> istr_Mant.Argumento[4]) THEN
	IF il_fila > 0 THEN
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	END IF
	
	
	idwc_envase.SetTransObject(SqlCa)
	IF idwc_envase.Retrieve(0) = 0 THEN
		idwc_envase.InsertRow(0)
	END IF
	
	il_fila = dw_2.InsertRow(0)
	Habilitaencab(False)
	pb_eliminar.enabled=TRUE
	dw_2.SetItem(il_fila,"lote_pltcod",dw_5.Object.plde_codigo[1])
	dw_2.SetItem(il_fila,"lote_espcod",dw_5.Object.espe_codigo[1])
	dw_2.ScrollToRow(il_fila)
	dw_2.SetRow(il_fila)
	dw_2.SetFocus()
	dw_2.SetColumn("lote_codigo")

ELSE
	MessageBox("Error","Debe elegir Camaras distintas")
END IF
end event

event ue_recuperadatos();call super::ue_recuperadatos;Long		ll_Fila, Respuesta, ll_Filaori, ll_Fila3
Integer  li_especie

IF istr_mant.Argumento[6] <> "" THEN
   DO
		ll_Fila	= dw_5.Retrieve(Integer(istr_Mant.Argumento[2]),81,  &
										 Integer(istr_Mant.Argumento[6]))
     IF ll_Fila = -1 THEN
			Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
	  ELSEIF ll_Fila > 0 THEN
         ll_Filaori	= dw_3.Retrieve(Integer(istr_Mant.Argumento[2]), 81, &
 												 Integer(istr_Mant.Argumento[6]))
	
			IF ll_Filaori = -1 THEN
				Respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
													Information!, RetryCancel!)
			
			END IF
			
			IF dw_3.RowCount()>0 THEN
				li_especie = dw_3.Object.lote_espcod[1]
				dw_5.Object.espe_codigo[1]= li_especie
			END IF
			
			HabilitaEncab(False)
	      dw_1.visible=False
			dw_3.visible=TRUE
			pb_lectura.Enabled   = FALSE
			pb_insertar.Enabled	= FALSE
			pb_eliminar.Enabled	= FALSE
			pb_grabar.Enabled		= FALSE
			pb_imprimir.Enabled	= True
			istr_mant.Solo_Consulta = TRUE
		ELSE
			MessageBox(	"Error de Consistencia", "Camara de Origen sin Datos.", &
							Information!)
		END IF
	
	LOOP WHILE Respuesta = 1
END IF
end event

event ue_borrar();call super::ue_borrar;IF dw_2.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_2.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

 IF dw_2.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_2.GetRow()
	END IF
END IF

end event

event ue_validaregistro();call super::ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_2.GetItemNumber(il_fila, "lote_codigo")) OR dw_2.GetItemNumber(il_fila, "lote_codigo") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nLote Código"
	ls_colu[li_cont]	= "lote_codigo"
END IF

IF Isnull(dw_2.GetItemNumber(il_fila, "lote_pltcod")) OR dw_2.GetItemNumber(il_fila, "lote_pltcod") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nLote Planta"
	ls_colu[li_cont]	= "lote_pltcod"
END IF

IF Isnull(dw_2.GetItemNumber(il_fila, "enva_tipoen")) OR dw_2.GetItemNumber(il_fila, "enva_tipoen") = 0 THEN
	li_cont ++
	ls_mensaje			= ls_mensaje + "~nTipo de Envase"
	ls_colu[li_cont]	= "enva_tipoen"
END IF

IF Isnull(dw_2.GetItemNumber(il_fila, "enva_codigo")) OR dw_2.GetItemNumber(il_fila, "enva_codigo") = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nEnvase"
	ls_colu[li_cont]	= "enva_codigo"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_2.SetColumn(ls_colu[1])
	dw_2.SetFocus()
	Message.DoubleParm = -1
	RETURN
END IF

IF dw_2.Object.caex_canbul[il_fila] = 0 THEN
	MessageBox("Error de Consistencia","La Cantidad En Existencia Es Igual a Cero para el Lote.")
	Message.DoubleParm = -1
	RETURN
END IF

IF dw_2.Object.tot_bultos[il_fila] <= 0 OR isnull(dw_2.Object.tot_bultos[il_fila]) THEN
	MessageBox("Error de Consistencia","La Cantidad a Traspasar debe ser Mayor a Cero.")
	Message.DoubleParm = -1
	RETURN
END IF

end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_spro_camexisfg_directa
boolean visible = false
integer x = 37
integer y = 28
integer width = 347
integer height = 68
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_spro_camexisfg_directa
integer x = 3287
integer y = 340
integer taborder = 40
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(True)

dw_2.Reset()
dw_1.Reset()
dw_3.Reset()
dw_5.Reset()

dw_1.visible=TRUE
dw_2.visible=TRUE
dw_3.visible=False

pb_insertar.Enabled  = TRUE
pb_eliminar.Enabled  = FALSE
pb_imprimir.Enabled  = FALSE

dw_5.InsertRow(0)
dw_5.SetItem(1, "plde_codigo", gstr_ParamPlanta.CodigoPlanta)
dw_5.SetItem(1, "mvce_fecmov",Today())
dw_5.SetItem(1, "espe_codigo", gstr_ParamPlanta.Codigoespecie)

istr_mant.argumento[3] = String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[3] = ""
istr_mant.argumento[4] = ""
istr_mant.argumento[6] = ""

dw_5.SetColumn("mvce_numero")
dw_5.SetFocus()


end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_spro_camexisfg_directa
boolean visible = false
integer x = 3287
integer y = 136
integer taborder = 20
boolean enabled = false
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_spro_camexisfg_directa
integer x = 3287
integer y = 680
integer taborder = 60
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_spro_camexisfg_directa
integer x = 3287
integer y = 548
integer taborder = 50
boolean enabled = true
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_spro_camexisfg_directa
integer x = 3287
integer taborder = 100
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_spro_camexisfg_directa
integer x = 3273
integer y = 932
integer taborder = 80
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_spro_camexisfg_directa
integer x = 3287
integer y = 824
integer taborder = 70
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_spro_camexisfg_directa
boolean visible = false
integer x = 146
integer y = 1132
integer width = 2962
integer height = 376
integer taborder = 0
string title = "Camara Destino"
string dataobject = "dw_mant_mues_spro_camexisfg_dir_dest"
boolean hscrollbar = true
boolean livescroll = false
end type

type dw_5 from datawindow within w_mant_mues_spro_camexisfg_directa
integer x = 389
integer y = 24
integer width = 2491
integer height = 440
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_mues_movtocamaraenca"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Null
String	ls_columna

ls_columna = GetColumnName()
SetNull(ls_Null)

CHOOSE CASE ls_columna

	CASE "mvce_numero"
		 IF existefolio(Long(data)) THEN
			istr_mant.argumento[6] = data
			parent.triggerevent("ue_recuperadatos")
		 ELSE
			This.SetItem(1, "mvce_numero", Long(ls_Null))
			This.SetFocus()
			RETURN 1
		 END IF	
		
	CASE "plde_codigo"
		IF NOT iuo_PlantaDesp.Existe(Integer(data),True,SqlCa) THEN
			This.SetItem(1, "plde_codigo", integer(ls_Null))
			This.SetFocus()
			RETURN 1
		ELSE
			ii_Planta	=	Integer(data)
		END IF

	CASE "cama_codori"
		IF NOT iuo_CamarasFrigoorig.Existe(gstr_paramplanta.codigoplanta,Integer(data),True,SqlCa) THEN
			This.SetItem(1, "cama_codori", integer(ls_Null))
			This.SetFocus()
			RETURN 1
		ELSE
			istr_Mant.Argumento[3]	=	Data
		END IF

	CASE "cama_coddes"
		IF NOT iuo_CamarasFrigodest.Existe(gstr_paramplanta.codigoplanta,Integer(data),True,SqlCa) THEN
			This.SetItem(1, "cama_coddes", integer(ls_Null))
			This.SetFocus()
			RETURN 1
		ELSE
			istr_Mant.Argumento[4]	=	Data
		END IF
		
	CASE "mvce_fecmov"
		istr_Mant.Argumento[5] = Data
END CHOOSE

end event

event itemerror;RETURN 1
end event

event buttonclicked;
CHOOSE CASE dwo.Name
	
	CASE "b_folio"
		buscafolio()		


END CHOOSE
end event

type dw_2 from datawindow within w_mant_mues_spro_camexisfg_directa
integer x = 91
integer y = 516
integer width = 3086
integer height = 1248
integer taborder = 30
boolean titlebar = true
string title = "Camara Origen"
string dataobject = "dw_mant_mues_spro_camexisfg_directa"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event clicked;//IF row > 0 THEN
//	IF IsSelected(row) THEN
//		SelectRow(row,False)
//	ELSE
//		SelectRow(row,True)
//	END IF
//END IF
//String ls_tecla
//
//IF Keydown(KeyShifT!) THEN
//	ls_tecla = "Shift"
//ELSEIF KeyDown(KeyControl!) THEN
//	ls_tecla = "Control"
//END IF
//
//F_Selecciona(This,ls_tecla,row)
//
//IF dw_2.GetSelectedRow(0) = 0 THEN
//	pb_grabar.Enabled	=	False
//ELSE
//	pb_grabar.Enabled	=	True
//END IF	
end event

event itemerror;RETURN 1
end event

event itemchanged;Long 		ll_Fila, li_nlotes, ll_loteco
String 	ls_Columna,ls_Null, ls_Lote, ls_lotepl, ls_lotees

SetNull(ls_Null)
CHOOSE CASE dwo.name
	CASE "lote_pltcod"
		IF NOT iuo_plantalote.existe(Integer(data),TRUE,SQLCA) AND BuscaDuplicado("lote_pltcod",Data) THEN
			this.SetITem(il_fila,"lote_pltcod",gstr_paramplanta.codigoplanta)
			RETURN 1
		END IF	
	
   CASE "lote_codigo"
		IF isnull(this.Object.lote_pltcod[il_fila]) THEN
			Messagebox("Error","Debe especificar una planta para el Lote") 
			this.SetITem(il_fila,"lote_codigo",integer(ls_Null))
			RETURN 1
		END IF
		IF BuscaDuplicado("lote_codigo",Data) THEN
			this.SetITem(il_fila,"lote_codigo",integer(ls_Null))
			RETURN 1
		END IF	
		ls_lotepl = string(this.Object.lote_pltcod[il_fila],'0000')
      ls_lotees =	string(this.Object.lote_pltcod[il_fila],'00')
		ll_loteco = long(Data)
		ls_lote   = ls_lotepl + ls_lotees + string(ll_loteco,'0000')
		
		li_nlotes = existelote (Integer(istr_mant.Argumento[2]), &
										Integer(istr_mant.Argumento[3]), ls_lote)
		IF li_nlotes= 0 THEN
			this.SetITem(il_fila,"lote_codigo",integer(ls_Null))
			RETURN 1
		END IF	
		IF li_nlotes > 0 THEN
			dw_2.SetItem(il_fila,"lote",ls_lote)
		END IF	
		IF li_nlotes = 1 THEN
			detlote(Integer(istr_mant.Argumento[2]), &
			  				    Integer(istr_mant.Argumento[3]), ls_lote)
			pb_grabar.Enabled=True						
		ELSEIF li_nlotes > 1 THEN
			Messagebox("Advertencia","Debe especificar el tipo de envase y envase") 
		END IF
		
	CASE "lote"
		IF len(data) = 10 THEN
			li_nlotes = existelote (Integer(istr_mant.Argumento[2]), &
				  				         Integer(istr_mant.Argumento[3]), data)
			IF li_nlotes = 1 THEN
				detlote(Integer(istr_mant.Argumento[2]), &
				  				    Integer(istr_mant.Argumento[3]), data)
				pb_grabar.Enabled=True						
			ELSEIF li_nlotes > 1 THEN
				Messagebox("Advertencia","Debe especificar el tipo de envase y envase") 
			END IF
		ELSE
			Messagebox("Error de Consistencia","Largo del Número del Lote es incorrecto") 
			Return
		END IF

	CASE "tot_bultos"
		IF Integer(Data) > 0 THEN			
			IF  dw_2.Object.caex_canbul[row] < Integer(Data)THEN
				MessageBox("Error", "Bultos A traspasar es Mayor a Bultos Existentes.",Exclamation! )			
				dw_2.SetItem(row,"tot_bultos",0)	
				RETURN -1
			END IF			
		ELSE
			MessageBox("Error", "Bultos A traspasar debe ser Mayor a Cero.",Exclamation! )			
			dw_2.SetItem(row,"tot_bultos",0)	
			RETURN 1
		END IF	
			
	
   CASE "enva_tipoen"
		dw_2.SetItem(il_fila,"enva_codigo",integer(ls_Null))
		IF NOT existetipoenva(integer(data)) AND BuscaDuplicado("enva_tipoen",Data) THEN
			dw_2.SetItem(il_fila,"enva_tipoen",integer(ls_Null))
			RETURN 1
		ELSE
			dw_2.GetChild("enva_codigo",idwc_envase)
			idwc_envase.SetTransObject(SQLCA)
			IF idwc_envase.Retrieve(integer(data)) = 0 THEN
				idwc_envase.InsertRow(0)
			END IF	
		END IF	
		
	CASE "enva_codigo"
      IF isnull(dw_2.Object.enva_tipoen[il_fila]) THEN
			messagebox("Error","Seleccione Primero un Tipo de Envase.")
			dw_2.SetItem(il_fila,"enva_codigo",integer(ls_Null))
			dw_2.SetItem(il_fila,"caex_canbul",integer(ls_Null))
			RETURN 1
		ELSE
			IF isnull(dw_2.Object.lote_codigo[il_fila]) THEN
				messagebox("Error","Seleccione Primero un Código de Lote.")
				dw_2.SetItem(il_fila,"enva_codigo",integer(ls_Null))
				dw_2.SetItem(il_fila,"enva_nombre",integer(ls_Null))
				dw_2.SetItem(il_fila,"caex_canbul",integer(ls_Null))
				RETURN 1
			ELSE
				IF BuscaDuplicado("enva_codigo",Data) THEN
					dw_2.SetItem(il_fila,"enva_codigo",integer(ls_Null))
					dw_2.SetItem(il_fila,"enva_nombre",integer(ls_Null))
					dw_2.SetItem(il_fila,"caex_canbul",integer(ls_Null))
					RETURN 1
				END IF
				
				IF NOT lotecondet(Integer(istr_mant.Argumento[2]), Integer(istr_mant.Argumento[3]), &
							dw_2.Object.lote[il_fila], dw_2.Object.enva_tipoen[il_fila], Integer(data) ) THEN
					RETURN 1		
				END IF			
			END IF				
		END IF 
		
	END CHOOSE		
		
end event

event doubleclicked;IF row > 0 THEN
	dw_2.Object.tot_bultos[row] = dw_2.Object.caex_canbul[row]
END IF
end event

event buttonclicked;CHOOSE CASE dwo.Name
	CASE "b_lote"
		buscalote()

END CHOOSE
end event

type dw_3 from datawindow within w_mant_mues_spro_camexisfg_directa
boolean visible = false
integer x = 91
integer y = 520
integer width = 3086
integer height = 1248
integer taborder = 90
boolean titlebar = true
string title = "Movimiento de Camara Detalle"
string dataobject = "dw_mant_mues_movtocamarafg"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_mant_mues_spro_camexisfg_directa
boolean visible = false
integer x = 169
integer y = 996
integer width = 2857
integer height = 432
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mant_mues_spro_camexisfg_dir_dest"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

