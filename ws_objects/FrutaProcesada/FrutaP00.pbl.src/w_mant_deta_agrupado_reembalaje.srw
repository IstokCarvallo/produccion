$PBExportHeader$w_mant_deta_agrupado_reembalaje.srw
forward
global type w_mant_deta_agrupado_reembalaje from w_mant_detalle
end type
type ids_palletencabupdate from datawindow within w_mant_deta_agrupado_reembalaje
end type
type ids_palletfrutanuevo from datawindow within w_mant_deta_agrupado_reembalaje
end type
type dw_4 from datawindow within w_mant_deta_agrupado_reembalaje
end type
type ids_palletencabnuevo from datawindow within w_mant_deta_agrupado_reembalaje
end type
end forward

global type w_mant_deta_agrupado_reembalaje from w_mant_detalle
integer width = 4091
integer height = 1736
string title = "CAMBIO DE CARACTERISTICAS REEMBALAJE"
ids_palletencabupdate ids_palletencabupdate
ids_palletfrutanuevo ids_palletfrutanuevo
dw_4 dw_4
ids_palletencabnuevo ids_palletencabnuevo
end type
global w_mant_deta_agrupado_reembalaje w_mant_deta_agrupado_reembalaje

type variables
DataWindowChild   dw_calibres		,	dw_productor 	,	&
                  dw_embalajes	,	dw_variedades 	,	idwc_embalaje, &
                  dw_plantadesp	,	dw_especies		,	idwc_variedad, idwc_calibrerot, idwc_calibre, &
                  dw_etiquetas	,	dw_condiciones	,	dw_cliente, idwc_etiqueta, &
						idwc_predio		, 	idwc_cuartel   ,  idwc_proddest, idwc_embdest, idwc_caldest, idwc_variedesdeta
						
Integer il_lote, il_cantidad, il_varirotu		

Str_mant2					istr_mant2
uo_calibre					iuo_calibre


end variables

forward prototypes
public function boolean existeembalaje (string as_embalaje)
public function boolean existecalibre (string as_calibre, long ai_fila)
public function boolean variedadrotula (integer codigo, integer especie)
public function boolean existevariedad (integer ai_variedad)
public function boolean existevarirotula (integer ai_variedad)
public function boolean existe_prodrotula (integer al_codigo)
end prototypes

public function boolean existeembalaje (string as_embalaje);integer li_cliente, li_count

li_cliente = Integer(dw_4.Object.clie_codigo[1])

SELECT count(*)
	INTO :li_count
	FROM dbo.embalajesprod 
	WHERE emba_codigo = :as_embalaje 
	AND  clie_codigo = :li_cliente;
		
If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla embalajesprod")
	Return False
End If	

If li_count = 0 Then
	MessageBox('Atención', 'El Embalaje Ingresado no Existe o Pertenece a otro Cliente.')
	Return False	
End If

Return True

end function

public function boolean existecalibre (string as_calibre, long ai_fila);integer li_count, li_especie, li_variedad
String	ls_calibre

li_especie = dw_1.Object.espe_codigo[1]
li_variedad = dw_4.Object.vari_codigo[ai_fila]

IF NOT iuo_calibre.existe(li_especie,li_variedad,as_calibre,True,sqlca) THEN
	RETURN False
ELSE
	ls_Calibre = iuo_calibre.calibre
END IF	

Return True

end function

public function boolean variedadrotula (integer codigo, integer especie);
SELECT vari_relaci
	INTO :il_varirotu
	FROM dbo.variedades
	WHERE espe_codigo = :especie
	AND vari_codigo    = :codigo;
 
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla variedades")
	RETURN FALSE
ELSE	
 	RETURN True
END IF


end function

public function boolean existevariedad (integer ai_variedad);integer li_cliente, li_count, li_especie

li_cliente = Integer(dw_4.Object.clie_codigo[1])
li_especie = dw_4.Object.espe_codigo[1]

SELECT count(*)
	INTO :li_count
	FROM dbo.variedades 
	WHERE vari_codigo = :ai_variedad 
	AND espe_codigo = :li_especie;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla variedades")
	Return False
END IF	

IF li_count = 0 THEN
	MessageBox('Atención', 'la Variedad Ingresada no Existe o Pertenece a otra Especie.')
	Return False	
END IF

Return True

end function

public function boolean existevarirotula (integer ai_variedad);integer li_cliente, li_count, li_especie

li_cliente = dw_4.Object.clie_codigo[1]
li_especie = dw_4.Object.espe_codigo[1]

SELECT count(*)
	INTO :li_count
	FROM dbo.variedades 
	WHERE vari_relaci = :ai_variedad 
	AND espe_codigo = :li_especie;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla variedades")
	Return True
END IF	

IF li_count = 0 THEN
	MessageBox('Atención', 'la Variedad Rotulada Ingresada no Existe o Pertenece a otra Especie.')
	Return True	
END IF

Return False

end function

public function boolean existe_prodrotula (integer al_codigo);integer li_count

SELECT count(*)
	INTO :li_count
	FROM dbo.productores 
	WHERE prod_codigo 	= :al_codigo;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Productores")
	Return True
END IF	

IF li_count = 0 THEN
	MessageBox('Atención', 'El Productor Rotulado no Existe en tabla Productores.')
	Return True	
END IF

Return False

end function

on w_mant_deta_agrupado_reembalaje.create
int iCurrent
call super::create
this.ids_palletencabupdate=create ids_palletencabupdate
this.ids_palletfrutanuevo=create ids_palletfrutanuevo
this.dw_4=create dw_4
this.ids_palletencabnuevo=create ids_palletencabnuevo
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.ids_palletencabupdate
this.Control[iCurrent+2]=this.ids_palletfrutanuevo
this.Control[iCurrent+3]=this.dw_4
this.Control[iCurrent+4]=this.ids_palletencabnuevo
end on

on w_mant_deta_agrupado_reembalaje.destroy
call super::destroy
destroy(this.ids_palletencabupdate)
destroy(this.ids_palletfrutanuevo)
destroy(this.dw_4)
destroy(this.ids_palletencabnuevo)
end on

event ue_deshace;call super::ue_deshace;//dw_1.Object.prod_codigo[il_fila]	=	Long(ias_campo[13])
//dw_1.Object.pafr_calibr[il_fila]	=	ias_campo[12]
//dw_1.Object.pafr_ccajas[il_fila]	=	Integer(ias_campo[11])
//dw_1.Object.pafr_nrlote[il_fila]	=	Integer(ias_campo[14])
end event

event open;x	= 100
y	= 450

PostEvent("ue_recuperadatos")

istr_mant2 = Message.PowerObjectParm
//istr_mant2.argumento[5] = istr_mant.argumento[5]

ids_palletencabupdate.SetTransObject(sqlca)
dw_4.SetTransObject(sqlca)
ids_palletfrutanuevo.SetTransObject(sqlca)
ids_palletencabnuevo.SetTransObject(sqlca)
istr_mant2.dw.ShareData(ids_palletencabupdate)
istr_mant2.dw2.ShareData(ids_palletfrutanuevo)
istr_mant2.dw3.ShareData(dw_4)
istr_mant2.dw4.ShareData(ids_palletencabnuevo)

iuo_calibre   						=	Create uo_calibre



end event

event ue_recuperadatos;Integer	li_especie
Long		ll_folio, ll_fila, ll_fila1, ll_Nuevo, ll_pallet
String	ls_variedad, ls_variedadrotu, ls_embalaje, ls_productor, ls_productorrotu, ls_calibre,&
			ls_calibrerotu, ls_cliente, ls_planta, ls_folio, ls_etiqueta, ls_embalajerotu
Date		ld_fecemb

ll_pallet		= dw_4.Object.paen_numero[1]
li_especie	= dw_4.Object.espe_codigo[1]	

If dw_4.RowCount() > 0 Then
	FOR ll_fila = 1 TO dw_4.RowCount()
		ls_variedadrotu 	= String(dw_4.Object.pafr_varrot[ll_fila])
		ls_embalaje 		= String(dw_4.Object.emba_codigo[ll_fila])
		ls_productor	 		= String(dw_4.Object.prod_codigo[ll_fila])		
		ls_calibre		 	= String(dw_4.Object.pafr_calibr[ll_fila])
		ls_variedad 			= String(dw_4.Object.vari_codigo[ll_fila])
		ls_embalajerotu 	= String(dw_4.Object.pafr_embrea[ll_fila])
		ls_productorrotu 	= String(dw_4.Object.pafr_prdrot[ll_fila])		
		ls_calibrerotu 		= String(dw_4.Object.pafr_calrot[ll_fila])
		ld_fecemb 			=  dw_4.Object.pafr_fecemb[ll_fila]
		ls_cliente				= String(dw_4.Object.clie_codigo[ll_fila])
		ls_folio				= String(dw_4.Object.paen_numero[ll_fila])
		ls_planta				= String(dw_4.Object.plde_codigo[ll_fila])
		
		ll_fila1	= dw_1.Find( "clie_codigo = " + ls_cliente + " AND paen_numero = " + ls_folio + &
								 " AND vari_codigo = " + ls_variedad + " AND pafr_varrot = " + ls_variedadrotu + &
								 " AND emba_codigo = '" + ls_embalaje + "'" + " AND pafr_embrea = '" + ls_embalajerotu + "'" + &
								 " AND pafr_prdrot = " + ls_productorrotu + " AND prod_codigo = " + ls_productor + &
								 " AND plde_codigo = " + ls_planta + &
								 " AND String(pafr_fecemb,'dd-mm-yyyy')	=	'" + String(ld_fecemb, "dd-mm-yyyy") + "'" + &
								 " AND pafr_calibr= '" + ls_calibre + "'" + " AND pafr_calrot = '" + ls_calibrerotu + "'", 1, dw_1.RowCount())
											 
		If ll_fila1 = 0   Then
			ll_Nuevo	=	dw_1.InsertRow(0)
			
			dw_1.Object.clie_codigo[ll_Nuevo]		= dw_4.Object.clie_codigo[ll_fila] 
			dw_1.Object.paen_numero[ll_Nuevo] =	dw_4.Object.paen_numero[ll_fila]    
			dw_1.Object.espe_codigo[ll_Nuevo] 	=	dw_4.Object.espe_codigo[ll_fila]		      
			dw_1.Object.vari_codigo[ll_Nuevo] 	=	dw_4.Object.vari_codigo[ll_fila]      
			dw_1.Object.emba_codigo[ll_Nuevo] 	= dw_4.Object.emba_codigo[ll_fila]     
			dw_1.Object.prod_codigo[ll_Nuevo] 	= dw_4.Object.prod_codigo[ll_fila]     
			dw_1.Object.cond_codigo[ll_Nuevo] 	= dw_4.Object.cond_codigo[ll_fila]     
			dw_1.Object.etiq_codigo[ll_Nuevo] 	= dw_4.Object.etiq_codigo[ll_fila]     
			dw_1.Object.plde_codigo[ll_Nuevo] 	= dw_4.Object.plde_codigo[ll_fila]     
			dw_1.Object.pafr_calibr[ll_Nuevo] 	= dw_4.Object.pafr_calibr[ll_fila]     
			dw_1.Object.pafr_ccajas[ll_Nuevo] 	= dw_4.Object.pafr_ccajas[ll_fila]    
			dw_1.Object.pafr_varrot[ll_Nuevo] 	= dw_4.Object.pafr_varrot[ll_fila]     
			dw_1.Object.pafr_prdrot[ll_Nuevo] 	= dw_4.Object.pafr_prdrot[ll_fila]     
			dw_1.Object.pafr_calrot[ll_Nuevo] 	= dw_4.Object.pafr_calrot[ll_fila]     
			dw_1.Object.pafr_fecemb[ll_Nuevo] 	= dw_4.Object.pafr_fecemb[ll_fila]     
			dw_1.Object.prod_nombre[ll_Nuevo] 	= dw_4.Object.prod_nombre[ll_fila]     
			dw_1.Object.vari_nombre[ll_Nuevo] 	= dw_4.Object.vari_nombre[ll_fila]  
			dw_1.Object.pafr_embrea[ll_Nuevo] 	= dw_4.Object.pafr_embrea[ll_fila]
			dw_1.Object.cate_codigo[ll_Nuevo] 	= dw_4.Object.cate_codigo[ll_fila]  
			dw_1.Object.pafr_catrot[ll_Nuevo] 	= dw_4.Object.pafr_catrot[ll_fila]
		ElseIf dw_1.Object.paen_numero[1] = dw_4.Object.paen_numero[ll_fila] Then
			dw_1.Object.pafr_ccajas[ll_fila1] 		= dw_1.Object.pafr_ccajas[ll_fila1] + dw_4.Object.pafr_ccajas[ll_fila]    
		End If							 
	NEXT	
	
	dw_1.GetChild("vari_codigo", idwc_variedad)
	idwc_variedad.SetTransObject(SqlCa)
	If idwc_variedad.Retrieve(dw_4.Object.espe_codigo[1]) = 0 Then
		idwc_variedad.InsertRow(0)
	End If
	
	dw_1.GetChild("vari_codigo", idwc_variedad)
	idwc_variedad.SetTransObject(SqlCa)
	If idwc_variedad.Retrieve(dw_4.Object.espe_codigo[1]) = 0 Then
		idwc_variedad.InsertRow(0)
	End If
	
	dw_1.GetChild("etiq_codigo", idwc_etiqueta)
	idwc_etiqueta.SetTransObject(SqlCa)
	If idwc_etiqueta.Retrieve() = 0 Then
		idwc_etiqueta.InsertRow(0)
	End If
	
	dw_1.GetChild("emba_codigo", idwc_embalaje)
	idwc_embalaje.SetTransObject(SqlCa)
	If idwc_embalaje.Retrieve() = 0 Then
		idwc_embalaje.InsertRow(0)
	End If
	
	dw_1.GetChild("pafr_calibr", idwc_calibre)
	idwc_calibre.SetTransObject(SqlCa)
	If idwc_calibre.Retrieve(li_especie) = 0 Then
		idwc_calibre.InsertRow(0)
	End If
	
	dw_1.GetChild("pafr_calrot", idwc_calibrerot)
	idwc_calibrerot.SetTransObject(SqlCa)
	If idwc_calibrerot.Retrieve(li_especie) = 0 Then
		idwc_calibrerot.InsertRow(0)
	End If
End If	


end event

type pb_ultimo from w_mant_detalle`pb_ultimo within w_mant_deta_agrupado_reembalaje
boolean visible = false
end type

type pb_siguiente from w_mant_detalle`pb_siguiente within w_mant_deta_agrupado_reembalaje
boolean visible = false
end type

type pb_anterior from w_mant_detalle`pb_anterior within w_mant_deta_agrupado_reembalaje
boolean visible = false
end type

type pb_primero from w_mant_detalle`pb_primero within w_mant_deta_agrupado_reembalaje
boolean visible = false
end type

type pb_cancela from w_mant_detalle`pb_cancela within w_mant_deta_agrupado_reembalaje
boolean visible = false
integer x = 3785
integer y = 400
end type

type pb_acepta from w_mant_detalle`pb_acepta within w_mant_deta_agrupado_reembalaje
integer x = 3790
integer y = 188
end type

event pb_acepta::clicked;istr_mant.respuesta = 0

CloseWithReturn(Parent, istr_mant2)
end event

type pb_salir from w_mant_detalle`pb_salir within w_mant_deta_agrupado_reembalaje
integer x = 3785
integer y = 620
end type

event pb_salir::clicked;istr_mant.respuesta = 0

CloseWithReturn(Parent, istr_mant2)
end event

type dw_1 from w_mant_detalle`dw_1 within w_mant_deta_agrupado_reembalaje
integer x = 9
integer y = 12
integer width = 3634
integer height = 1592
string dataobject = "dw_mues_palletfruta_repaletizaje_caja"
boolean hscrollbar = true
boolean vscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna, ls_null, ls_variedad, ls_variedadrotu, ls_embalaje, ls_productor, &
			ls_productorrotu, ls_calibre, ls_calibrerotu, ls_cliente, ls_planta, ls_folio, &
			ls_etiqueta, ls_embalajerotu,ls_variedad1, ls_variedadrotu1, ls_embalaje1, ls_productor1, &
			ls_productorrotu1, ls_calibre1, ls_calibrerotu1, ls_cliente1, ls_planta1, ls_folio1, &
			ls_etiqueta1, ls_embalajerotu1
Long 		ll_fila, ll_fila1, ll_fila2, ll_paen_nroori, ll_paen_numero, ll_pafr_secuen, &
			ll_pafr_secuen1, ll_existe, ll_cont, ll_actualiza
Integer  li_cliente,li_cliente1
Date		ld_fecemb,ld_fecemb1

ls_variedadrotu 	= String(dw_1.Object.pafr_varrot[row])
ls_embalaje 		= String(dw_1.Object.emba_codigo[row])
ls_productor	 		= String(dw_1.Object.prod_codigo[row])		
ls_calibre		 	= String(dw_1.Object.pafr_calibr[row])
ls_variedad 			= String(dw_1.Object.vari_codigo[row])
ls_embalajerotu 	= String(dw_1.Object.pafr_embrea[row])
ls_productorrotu 	= String(dw_1.Object.pafr_prdrot[row])		
ls_calibrerotu 		= String(dw_1.Object.pafr_calrot[row])
ld_fecemb 			=  dw_1.Object.pafr_fecemb[row]
ls_cliente				= String(dw_1.Object.clie_codigo[row])
ls_folio				= String(dw_1.Object.paen_numero[row])
ls_planta				= String(dw_1.Object.plde_codigo[row])

SetNull(ls_Null)

li_cliente = dw_4.Object.clie_codigo[1]
ls_Columna = dwo.name

Choose Case ls_Columna
	Case "emba_codigo"
		MessageBox("Atención", "VerIfique Tipo pallet.")
		
		If NOT existeembalaje(data) Then
			This.SetItem(Row, 'emba_codigo', String(ls_Null))
			Return 1
		End If	
		
		If il_cantidad = 0 Then
			FOR ll_fila1 = 1 TO ids_palletencabnuevo.RowCount()
				ll_paen_numero = ids_palletencabnuevo.Object.paen_numero[ll_fila1]
				ll_paen_nroori = dw_1.Object.paen_numero[Row]
				If ll_paen_numero = ll_paen_nroori Then
					ids_palletencabnuevo.Object.emba_codigo[ll_fila1] = data
				End If
			NEXT
		End If	
		
		FOR ll_fila1 = 1 TO ids_palletencabupdate.RowCount()
			ll_paen_numero = ids_palletencabupdate.Object.paen_numero[ll_fila1]
			ll_paen_nroori = dw_1.Object.paen_numero[Row]
			If ll_paen_numero = ll_paen_nroori Then
				ids_palletencabupdate.Object.emba_codigo[ll_fila1] = data
			End If
		NEXT
		
		FOR ll_actualiza = 1 TO dw_4.RowCount()
			ll_existe	= dw_4.Find( "clie_codigo = " + ls_cliente + " AND paen_numero = " + ls_folio + &
							 " AND vari_codigo = " + ls_variedad + " AND pafr_varrot = " + ls_variedadrotu + &
							 " AND emba_codigo = '" + ls_embalaje + "'" + " AND pafr_embrea = '" + ls_embalajerotu + "'" + &
							 " AND pafr_prdrot = " + ls_productorrotu + " AND prod_codigo = " + ls_productor + &
							 " AND plde_codigo = " + ls_planta + &
							 " AND String(pafr_fecemb,'dd-mm-yyyy')	=	'" + String(ld_fecemb, "dd-mm-yyyy") + "'" + &
							 " AND pafr_calibr= '" + ls_calibre + "'" + " AND pafr_calrot = '" + ls_calibrerotu + "'", 1, dw_4.RowCount())
							 
			If ll_existe > 0 Then
				dw_4.Object.emba_codigo[ll_existe] = Data
			End If	
		NEXT	
			
		FOR ll_fila2 = 1 TO ids_palletfrutanuevo.RowCount()
			ls_variedadrotu1 	= String(ids_palletfrutanuevo.Object.pafr_varrot[ll_fila2])
			ls_embalaje1 		= String(ids_palletfrutanuevo.Object.emba_codigo[ll_fila2])
			ls_productor1	 	= String(ids_palletfrutanuevo.Object.prod_codigo[ll_fila2])		
			ls_calibre1		 	= String(ids_palletfrutanuevo.Object.pafr_calibr[ll_fila2])
			ls_variedad1 		= String(ids_palletfrutanuevo.Object.vari_codigo[ll_fila2])
			ls_embalajerotu1 	= String(ids_palletfrutanuevo.Object.pafr_embrea[ll_fila2])
			ls_productorrotu1	= String(ids_palletfrutanuevo.Object.pafr_prdrot[ll_fila2])		
			ls_calibrerotu1 		= String(ids_palletfrutanuevo.Object.pafr_calrot[ll_fila2])
			ld_fecemb1 			=  ids_palletfrutanuevo.Object.pafr_fecemb[ll_fila2]
			ls_cliente1			= String(ids_palletfrutanuevo.Object.clie_codigo[ll_fila2])
			ls_folio1				= String(ids_palletfrutanuevo.Object.paen_numero[ll_fila2])
			ls_planta1			= String(ids_palletfrutanuevo.Object.plde_codigo[ll_fila2])
			ll_paen_nroori 		= ids_palletfrutanuevo.Object.paen_numero[ll_fila2]
			ll_paen_numero 	= dw_1.Object.paen_numero[Row]
		
			If ll_paen_nroori = ll_paen_numero AND ls_embalaje = ls_embalaje1 AND &
				ls_variedadrotu1 = ls_variedadrotu AND ls_productor1 = ls_productor AND &
				ls_calibre1 = ls_calibre AND ls_variedad1 = ls_variedad AND &
				ls_embalajerotu1 = ls_embalajerotu AND ls_productorrotu1 = ls_productorrotu AND &
				ls_calibrerotu1 = ls_calibrerotu AND ld_fecemb1 = ld_fecemb AND &
				ls_cliente1 = ls_cliente AND ls_planta1 = ls_planta Then
				
					ids_palletfrutanuevo.Object.emba_codigo[ll_fila2] = data
			End If
		NEXT
	
		istr_mant2.Argumento[5] = Data //embalaje
		
  Case "pafr_calibr"
		If NOT existecalibre(data,row) Then
			This.SetItem(Row, 'pafr_calibr', ls_Null)
			Return 1
		End If	
		
		istr_mant2.Argumento[8] = MID((TRIM(data)+'   '),1,3)
		
		FOR ll_actualiza = 1 TO dw_4.RowCount()		
			ll_existe	= dw_4.Find( "clie_codigo = " + ls_cliente + " AND paen_numero = " + ls_folio + &
							 " AND vari_codigo = " + ls_variedad + " AND pafr_varrot = " + ls_variedadrotu + &
							 " AND emba_codigo = '" + ls_embalaje + "'" + " AND pafr_embrea = '" + ls_embalajerotu + "'" + &
							 " AND pafr_prdrot = " + ls_productorrotu + " AND prod_codigo = " + ls_productor + &
							 " AND plde_codigo = " + ls_planta + &
							 " AND String(pafr_fecemb,'dd-mm-yyyy')	=	'" + String(ld_fecemb, "dd-mm-yyyy") + "'" + &
							 " AND pafr_calibr= '" + ls_calibre + "'" + " AND pafr_calrot = '" + ls_calibrerotu + "'", 1, dw_4.RowCount())
			If ll_existe > 0 Then
				dw_4.Object.pafr_calibr[ll_existe] = iuo_calibre.fomato_calibre(data,sqlca)
			End If	
		NEXT	
					
		FOR ll_fila2 = 1 TO ids_palletfrutanuevo.RowCount()
			ls_variedadrotu1 	= String(ids_palletfrutanuevo.Object.pafr_varrot[ll_fila2])
			ls_embalaje1 		= String(ids_palletfrutanuevo.Object.emba_codigo[ll_fila2])
			ls_productor1	 	= String(ids_palletfrutanuevo.Object.prod_codigo[ll_fila2])		
			ls_calibre1		 	= String(ids_palletfrutanuevo.Object.pafr_calibr[ll_fila2])
			ls_variedad1 		= String(ids_palletfrutanuevo.Object.vari_codigo[ll_fila2])
			ls_embalajerotu1 	= String(ids_palletfrutanuevo.Object.pafr_embrea[ll_fila2])
			ls_productorrotu1	= String(ids_palletfrutanuevo.Object.pafr_prdrot[ll_fila2])		
			ls_calibrerotu1 		= String(ids_palletfrutanuevo.Object.pafr_calrot[ll_fila2])
			ld_fecemb1 			=  ids_palletfrutanuevo.Object.pafr_fecemb[ll_fila2]
			ls_cliente1			= String(ids_palletfrutanuevo.Object.clie_codigo[ll_fila2])
			ls_folio1				= String(ids_palletfrutanuevo.Object.paen_numero[ll_fila2])
			ls_planta1			= String(ids_palletfrutanuevo.Object.plde_codigo[ll_fila2])
			
			ll_paen_nroori = ids_palletfrutanuevo.Object.paen_numero[ll_fila2]
			ll_paen_numero = dw_1.Object.paen_numero[Row]
		
			If ll_paen_nroori = ll_paen_numero AND &
				ls_embalaje = ls_embalaje1 AND ls_variedadrotu1 = ls_variedadrotu AND &
				ls_productor1 = ls_productor AND ls_calibre1 = ls_calibre AND &
				ls_variedad1 = ls_variedad AND ls_embalajerotu1 = ls_embalajerotu AND &
				ls_productorrotu1 = ls_productorrotu AND ls_calibrerotu1 = ls_calibrerotu AND &
				ld_fecemb1 = ld_fecemb AND ls_cliente1 = ls_cliente AND ls_planta1 = ls_planta Then
				
				ids_palletfrutanuevo.Object.pafr_calibr[ll_fila2] = data
				
			End If
		NEXT
		Return 1
		
	Case "pafr_calrot"
		If NOT existecalibre(data,row) Then
			This.SetItem(Row, 'pafr_calrot', ls_Null)
			Return 1
		End If	
		
		FOR ll_actualiza = 1 TO dw_4.RowCount()
			ll_existe	= dw_4.Find( "clie_codigo = " + ls_cliente + " AND paen_numero = " + ls_folio + &
							 " AND vari_codigo = " + ls_variedad + " AND pafr_varrot = " + ls_variedadrotu + &
							 " AND emba_codigo = '" + ls_embalaje + "'" + " AND pafr_embrea = '" + ls_embalajerotu + "'" + &
							 " AND pafr_prdrot = " + ls_productorrotu + " AND prod_codigo = " + ls_productor + &
							 " AND plde_codigo = " + ls_planta + &
							 " AND String(pafr_fecemb,'dd-mm-yyyy')	=	'" + String(ld_fecemb, "dd-mm-yyyy") + "'" + &
							 " AND pafr_calibr= '" + ls_calibre + "'" + " AND pafr_calrot = '" + ls_calibrerotu + "'", 1, dw_4.RowCount())
							 
			If ll_existe > 0 Then
				dw_4.Object.pafr_calrot[ll_existe] = MID((TRIM(data)+'   '),1,3)
			End If	
		NEXT	
		
		FOR ll_fila2 = 1 TO ids_palletfrutanuevo.RowCount()
			ls_variedadrotu1 	= String(ids_palletfrutanuevo.Object.pafr_varrot[ll_fila2])
			ls_embalaje1 		= String(ids_palletfrutanuevo.Object.emba_codigo[ll_fila2])
			ls_productor1	 	= String(ids_palletfrutanuevo.Object.prod_codigo[ll_fila2])		
			ls_calibre1		 	= String(ids_palletfrutanuevo.Object.pafr_calibr[ll_fila2])
			ls_variedad1 		= String(ids_palletfrutanuevo.Object.vari_codigo[ll_fila2])
			ls_embalajerotu1 	= String(ids_palletfrutanuevo.Object.pafr_embrea[ll_fila2])
			ls_productorrotu1	= String(ids_palletfrutanuevo.Object.pafr_prdrot[ll_fila2])		
			ls_calibrerotu1 		= String(ids_palletfrutanuevo.Object.pafr_calrot[ll_fila2])
			ld_fecemb1 			=  ids_palletfrutanuevo.Object.pafr_fecemb[ll_fila2]
			ls_cliente1			= String(ids_palletfrutanuevo.Object.clie_codigo[ll_fila2])
			ls_folio1				= String(ids_palletfrutanuevo.Object.paen_numero[ll_fila2])
			ls_planta1			= String(ids_palletfrutanuevo.Object.plde_codigo[ll_fila2])
			
			ll_paen_nroori = ids_palletfrutanuevo.Object.paen_numero[ll_fila2]
			ll_paen_numero = dw_1.Object.paen_numero[Row]
		
			If ll_paen_nroori = ll_paen_numero AND ls_embalaje = ls_embalaje1 AND &
				ls_variedadrotu1 = ls_variedadrotu AND ls_productor1 = ls_productor AND &
				ls_calibre1 = ls_calibre AND ls_variedad1 = ls_variedad AND &
				ls_embalajerotu1 = ls_embalajerotu AND ls_productorrotu1 = ls_productorrotu AND &
				ls_calibrerotu1 = ls_calibrerotu AND ld_fecemb1 = ld_fecemb AND &
				ls_cliente1 = ls_cliente AND ls_planta1 = ls_planta Then
				
				ids_palletfrutanuevo.Object.pafr_calrot[ll_fila2] = data
				
			End If
		NEXT
		
	Case "vari_codigo"
		FOR ll_actualiza = 1 TO dw_4.RowCount()
			ll_existe	= dw_4.Find( "clie_codigo = " + ls_cliente + " AND paen_numero = " + ls_folio + &
							 " AND vari_codigo = " + ls_variedad + " AND pafr_varrot = " + ls_variedadrotu + &
							 " AND emba_codigo = '" + ls_embalaje + "'" + " AND pafr_embrea = '" + ls_embalajerotu + "'" + &
							 " AND pafr_prdrot = " + ls_productorrotu + " AND prod_codigo = " + ls_productor + &
							 " AND plde_codigo = " + ls_planta + &
							 " AND String(pafr_fecemb,'dd-mm-yyyy')	=	'" + String(ld_fecemb, "dd-mm-yyyy") + "'" + &
							 " AND pafr_calibr= '" + ls_calibre + "'" + " AND pafr_calrot = '" + ls_calibrerotu + "'", 1, dw_4.RowCount())
							 
			If ll_existe > 0 Then
				dw_4.Object.vari_codigo[ll_existe] = Integer(Data)
				
				If variedadrotula(Integer(data),dw_4.Object.espe_codigo[ll_existe]) Then
					dw_4.Object.pafr_varrot[ll_existe] = il_varirotu
				Else	
					dw_4.Object.pafr_varrot[ll_existe] = Integer(Data)
				End If
				
				If NOT existevariedad(Integer(data)) Then
					This.SetItem(Row, 'vari_codigo', Integer(ls_Null))
					Return 1
				End If	
			End If
		NEXT
		
		FOR ll_fila2 = 1 TO ids_palletfrutanuevo.RowCount()
			ls_variedadrotu1 	= String(ids_palletfrutanuevo.Object.pafr_varrot[ll_fila2])
			ls_embalaje1 		= String(ids_palletfrutanuevo.Object.emba_codigo[ll_fila2])
			ls_productor1	 	= String(ids_palletfrutanuevo.Object.prod_codigo[ll_fila2])		
			ls_calibre1		 	= String(ids_palletfrutanuevo.Object.pafr_calibr[ll_fila2])
			ls_variedad1 		= String(ids_palletfrutanuevo.Object.vari_codigo[ll_fila2])
			ls_embalajerotu1 	= String(ids_palletfrutanuevo.Object.pafr_embrea[ll_fila2])
			ls_productorrotu1	= String(ids_palletfrutanuevo.Object.pafr_prdrot[ll_fila2])		
			ls_calibrerotu1 		= String(ids_palletfrutanuevo.Object.pafr_calrot[ll_fila2])
			ld_fecemb1 			=  ids_palletfrutanuevo.Object.pafr_fecemb[ll_fila2]
			ls_cliente1			= String(ids_palletfrutanuevo.Object.clie_codigo[ll_fila2])
			ls_folio1				= String(ids_palletfrutanuevo.Object.paen_numero[ll_fila2])
			ls_planta1			= String(ids_palletfrutanuevo.Object.plde_codigo[ll_fila2])
			
			ll_paen_nroori = ids_palletfrutanuevo.Object.paen_numero[ll_fila2]
			ll_paen_numero = dw_1.Object.paen_numero[Row]
		
			If ll_paen_nroori = ll_paen_numero AND ls_embalaje = ls_embalaje1 AND &
				ls_variedadrotu1 = ls_variedadrotu AND ls_productor1 = ls_productor AND &
				ls_calibre1 = ls_calibre AND ls_variedad1 = ls_variedad AND &
				ls_embalajerotu1 = ls_embalajerotu AND ls_productorrotu1 = ls_productorrotu AND &
				ls_calibrerotu1 = ls_calibrerotu AND ld_fecemb1 = ld_fecemb AND &
				ls_cliente1 = ls_cliente AND ls_planta1 = ls_planta Then
				
				ids_palletfrutanuevo.Object.vari_codigo[ll_fila2] = Integer(data)
				ids_palletencabnuevo.Object.paen_varrot[ll_fila2] = il_varirotu
			End If
		NEXT
					
		If il_cantidad = 0 Then
			FOR ll_fila1 = 1 TO ids_palletencabnuevo.RowCount()
				ll_paen_numero = ids_palletencabnuevo.Object.paen_numero[ll_fila1]
				ll_paen_nroori = dw_4.Object.paen_numero[1]
				If ll_paen_numero = ll_paen_nroori Then
					ids_palletencabnuevo.Object.vari_codigo[ll_fila1] = Integer(data)
					ids_palletencabnuevo.Object.paen_varrot[ll_fila1] = il_varirotu
				End If
			NEXT
		End If	
		
		FOR ll_fila1 = 1 TO ids_palletencabupdate.RowCount()
			ll_paen_numero = ids_palletencabupdate.Object.paen_numero[ll_fila1]
			ll_paen_nroori = dw_4.Object.paen_numero[1]
			If ll_paen_numero = ll_paen_nroori Then
				ids_palletencabupdate.Object.vari_codigo[ll_fila1] = Integer(data)
				ids_palletencabupdate.Object.paen_varrot[ll_fila1] = il_varirotu
			End If
		NEXT
		
		istr_mant2.Argumento[6] = (data) //variedad encabezado
		istr_mant2.Argumento[7] = String(il_varirotu) //variedad rotualda encabezado
		
	Case "pafr_varrot"
		If ExisteVariRotula(Integer(data)) Then
			This.SetItem(Row, 'pafr_varrot', Integer(ls_Null)) 
			Return 1
		End If	
		
		FOR ll_actualiza = 1 TO dw_4.RowCount()	
			ll_existe	= dw_4.Find( "clie_codigo = " + ls_cliente + " AND paen_numero = " + ls_folio + &
							 " AND vari_codigo = " + ls_variedad + " AND pafr_varrot = " + ls_variedadrotu + &
							 " AND emba_codigo = '" + ls_embalaje + "'" + " AND pafr_embrea = '" + ls_embalajerotu + "'" + &
							 " AND pafr_prdrot = " + ls_productorrotu + " AND prod_codigo = " + ls_productor + &
							 " AND plde_codigo = " + ls_planta + &
							 " AND String(pafr_fecemb,'dd-mm-yyyy')	=	'" + String(ld_fecemb, "dd-mm-yyyy") + "'" + &
							 " AND pafr_calibr= '" + ls_calibre + "'" + " AND pafr_calrot = '" + ls_calibrerotu + "'", 1, dw_4.RowCount())
							 
			If ll_existe > 0 Then dw_4.Object.pafr_varrot[ll_existe] = Integer(Data)

		NEXT	
		
		FOR ll_fila2 = 1 TO ids_palletfrutanuevo.RowCount()
			ls_variedadrotu1 	= String(ids_palletfrutanuevo.Object.pafr_varrot[ll_fila2])
			ls_embalaje1 		= String(ids_palletfrutanuevo.Object.emba_codigo[ll_fila2])
			ls_productor1	 	= String(ids_palletfrutanuevo.Object.prod_codigo[ll_fila2])		
			ls_calibre1		 	= String(ids_palletfrutanuevo.Object.pafr_calibr[ll_fila2])
			ls_variedad1 		= String(ids_palletfrutanuevo.Object.vari_codigo[ll_fila2])
			ls_embalajerotu1 	= String(ids_palletfrutanuevo.Object.pafr_embrea[ll_fila2])
			ls_productorrotu1	= String(ids_palletfrutanuevo.Object.pafr_prdrot[ll_fila2])		
			ls_calibrerotu1 		= String(ids_palletfrutanuevo.Object.pafr_calrot[ll_fila2])
			ld_fecemb1 			=  ids_palletfrutanuevo.Object.pafr_fecemb[ll_fila2]
			ls_cliente1			= String(ids_palletfrutanuevo.Object.clie_codigo[ll_fila2])
			ls_folio1				= String(ids_palletfrutanuevo.Object.paen_numero[ll_fila2])
			ls_planta1			= String(ids_palletfrutanuevo.Object.plde_codigo[ll_fila2])
			
			ll_paen_nroori 		= ids_palletfrutanuevo.Object.paen_numero[ll_fila2]
			ll_paen_numero	= dw_1.Object.paen_numero[Row]
		
			If ll_paen_nroori = ll_paen_numero AND ls_embalaje = ls_embalaje1 AND &
				ls_variedadrotu1 = ls_variedadrotu AND ls_productor1 = ls_productor AND &
				ls_calibre1 = ls_calibre AND ls_variedad1 = ls_variedad AND &
				ls_embalajerotu1 = ls_embalajerotu AND ls_productorrotu1 = ls_productorrotu AND &
				ls_calibrerotu1 = ls_calibrerotu AND ld_fecemb1 = ld_fecemb AND &
				ls_cliente1 = ls_cliente AND ls_planta1 = ls_planta Then
				
				ids_palletfrutanuevo.Object.pafr_varrot[ll_fila2] = Integer(data)
			End If
		NEXT
						
	Case "pafr_prdrot"
		If existe_prodrotula(Long(data)) Then
			This.SetItem(Row, 'pafr_prdrot', Long(ls_Null)) 
			Return 1
		End If	
		
		FOR ll_actualiza = 1 TO dw_4.RowCount()
			ll_existe	= dw_4.Find( "clie_codigo = " + ls_cliente + " AND paen_numero = " + ls_folio + &
							 " AND vari_codigo = " + ls_variedad + " AND pafr_varrot = " + ls_variedadrotu + &
							 " AND emba_codigo = '" + ls_embalaje + "'" + " AND pafr_embrea = '" + ls_embalajerotu + "'" + &
							 " AND pafr_prdrot = " + ls_productorrotu + " AND prod_codigo = " + ls_productor + &
							 " AND plde_codigo = " + ls_planta + &
							 " AND String(pafr_fecemb,'dd-mm-yyyy')	=	'" + String(ld_fecemb, "dd-mm-yyyy") + "'" + &
							 " AND pafr_calibr= '" + ls_calibre + "'" + " AND pafr_calrot = '" + ls_calibrerotu + "'", 1, dw_4.RowCount())
							 
			If ll_existe > 0 Then
				dw_4.Object.pafr_prdrot[ll_existe] = Long(Data)
			End If	
		NEXT	
		
		FOR ll_fila2 = 1 TO ids_palletfrutanuevo.RowCount()
			ls_variedadrotu1 	= String(ids_palletfrutanuevo.Object.pafr_varrot[ll_fila2])
			ls_embalaje1 		= String(ids_palletfrutanuevo.Object.emba_codigo[ll_fila2])
			ls_productor1	 	= String(ids_palletfrutanuevo.Object.prod_codigo[ll_fila2])		
			ls_calibre1		 	= String(ids_palletfrutanuevo.Object.pafr_calibr[ll_fila2])
			ls_variedad1 		= String(ids_palletfrutanuevo.Object.vari_codigo[ll_fila2])
			ls_embalajerotu1 	= String(ids_palletfrutanuevo.Object.pafr_embrea[ll_fila2])
			ls_productorrotu1	= String(ids_palletfrutanuevo.Object.pafr_prdrot[ll_fila2])		
			ls_calibrerotu1 		= String(ids_palletfrutanuevo.Object.pafr_calrot[ll_fila2])
			ld_fecemb1 			=  ids_palletfrutanuevo.Object.pafr_fecemb[ll_fila2]
			ls_cliente1			= String(ids_palletfrutanuevo.Object.clie_codigo[ll_fila2])
			ls_folio1				= String(ids_palletfrutanuevo.Object.paen_numero[ll_fila2])
			ls_planta1			= String(ids_palletfrutanuevo.Object.plde_codigo[ll_fila2])
			
			ll_paen_nroori = ids_palletfrutanuevo.Object.paen_numero[ll_fila2]
			ll_paen_numero = dw_1.Object.paen_numero[Row]
		
			If ll_paen_nroori = ll_paen_numero AND ls_embalaje = ls_embalaje1 AND &
				ls_variedadrotu1 = ls_variedadrotu AND ls_productor1 = ls_productor AND &
				ls_calibre1 = ls_calibre AND ls_variedad1 = ls_variedad AND &
				ls_embalajerotu1 = ls_embalajerotu AND ls_productorrotu1 = ls_productorrotu AND &
				ls_calibrerotu1 = ls_calibrerotu AND ld_fecemb1 = ld_fecemb AND &
				ls_cliente1 = ls_cliente AND ls_planta1 = ls_planta Then
				
				ids_palletfrutanuevo.Object.pafr_prdrot[ll_fila2] = Long(data)
			End If
		NEXT
End Choose

end event

event dw_1::clicked;call super::clicked;String	ls_columna

end event

type ids_palletencabupdate from datawindow within w_mant_deta_agrupado_reembalaje
boolean visible = false
integer x = 1367
integer y = 600
integer width = 2391
integer height = 672
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_palletencab_repaletizaje2"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type ids_palletfrutanuevo from datawindow within w_mant_deta_agrupado_reembalaje
boolean visible = false
integer x = 818
integer y = 492
integer width = 2176
integer height = 700
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_palletfruta_repaletizaje2"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_mant_deta_agrupado_reembalaje
boolean visible = false
integer y = 1020
integer width = 3095
integer height = 800
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_palletfruta_repaletizaje"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type ids_palletencabnuevo from datawindow within w_mant_deta_agrupado_reembalaje
boolean visible = false
integer x = 91
integer y = 648
integer width = 686
integer height = 400
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mant_palletencab_repaletizaje2"
boolean resizable = true
boolean livescroll = true
end type

