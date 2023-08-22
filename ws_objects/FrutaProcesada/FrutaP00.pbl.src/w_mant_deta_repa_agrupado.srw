$PBExportHeader$w_mant_deta_repa_agrupado.srw
forward
global type w_mant_deta_repa_agrupado from w_mant_detalle_csd
end type
type dw_2 from datawindow within w_mant_deta_repa_agrupado
end type
type dw_3 from datawindow within w_mant_deta_repa_agrupado
end type
end forward

global type w_mant_deta_repa_agrupado from w_mant_detalle_csd
integer width = 4174
integer height = 1824
string title = "TRASPASO DE CAJAS"
dw_2 dw_2
dw_3 dw_3
end type
global w_mant_deta_repa_agrupado w_mant_deta_repa_agrupado

type variables
DataWindowChild   dw_calibres		,	dw_productor 	,	&
                  dw_embalajes	,	dw_variedades 	,	&
                  dw_plantadesp	,	dw_especies		,	&
                  dw_etiquetas	,	dw_condiciones	,	dw_cliente,&
						idwc_predio		, 	idwc_cuartel
						
Integer il_lote						


end variables

on w_mant_deta_repa_agrupado.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.dw_3
end on

on w_mant_deta_repa_agrupado.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.dw_3)
end on

event ue_deshace;call super::ue_deshace;//dw_1.Object.prod_codigo[il_fila]	=	Long(ias_campo[13])
//dw_1.Object.pafr_calibr[il_fila]	=	ias_campo[12]
//dw_1.Object.pafr_ccajas[il_fila]	=	Integer(ias_campo[11])
//dw_1.Object.pafr_nrlote[il_fila]	=	Integer(ias_campo[14])
end event

event open;x	= 100
y	= 450

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm
istr_mant.argumento[5] = istr_mant.argumento[5]

dw_2.SetTransObject(sqlca)
dw_1.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_2)
istr_mant.dw2.ShareData(dw_3)


end event

event ue_recuperadatos;Integer	li_cliente,li_planta
Long		ll_folio, ll_fila, ll_fila1, ll_fila2
String	ls_variedad, ls_embalaje, ls_productor, ls_calibre, ls_cliente, ls_planta, ls_folio
Date		ld_fecemb

ll_folio 	=	Long(istr_mant.argumento[5])
li_cliente	=	Integer(istr_Mant.Argumento[1])
li_planta	=	Integer(istr_Mant.Argumento[2])

ls_cliente	=	istr_Mant.Argumento[1]
ls_planta	=	istr_Mant.Argumento[2]
ls_folio 	=	istr_mant.argumento[5]

ll_fila2 =dw_1.Retrieve(li_cliente,li_planta,ll_folio)

IF ll_fila2 > 0 THEN
	IF dw_3.RowCount() > 0 THEN
			
		FOR ll_fila = 1 TO dw_3.RowCount()
				ls_variedad 	= String(dw_3.Object.pafr_varrot[ll_fila])
				ls_embalaje	= String(dw_3.Object.emba_codigo[ll_fila])
				ls_productor	= String(dw_3.Object.pafr_prdrot[ll_fila])		
				ls_calibre 	= String(dw_3.Object.pafr_calrot[ll_fila])	
				
				ld_fecemb =  dw_3.Object.pafr_fecemb[ll_fila]
				
				ll_fila1	= dw_1.Find( "clie_codigo = " + ls_cliente + &
										 " AND paen_numero = " + ls_folio + &
										 " AND pafr_varrot = " + ls_variedad + &
										 " AND emba_codigo = '" + ls_embalaje + "'" + &
										 " AND pafr_prdrot = " + ls_productor + &
										 " AND plde_codigo = " + ls_planta + &
										 " AND cajas <> " + '0' + &
										 " AND String(pafr_fecemb,'dd-mm-yyyy')	=	'" + String(ld_fecemb, "dd-mm-yyyy") + "'" + &
										 " AND pafr_calrot = '" + ls_calibre + "'", 1, dw_1.RowCount())
		
			IF ll_fila1 > 0 AND dw_1.Object.paen_numero[1] = dw_3.Object.paen_numero[ll_fila]  THEN
				dw_1.Object.cajas[ll_fila1] = dw_1.Object.cajas[ll_fila1] - dw_3.Object.caja_traspa[ll_fila]
			END IF	
										 
		NEXT								 
	END IF	
ELSE
	MessageBox("Atención", "Revise EL predio y Cuartel del Pallet.", &
					Exclamation!, OK!)
	Return				
END IF

end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_repa_agrupado
boolean visible = false
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_repa_agrupado
boolean visible = false
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_repa_agrupado
boolean visible = false
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_repa_agrupado
boolean visible = false
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_repa_agrupado
boolean visible = false
integer x = 3863
integer y = 400
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_repa_agrupado
integer x = 3863
integer y = 188
end type

event pb_acepta::clicked;Long	ll_fila, ll_fila1, ll_fila2, ll_fil
String	ls_embalaje, ls_productor, ls_cliente, ls_folio, ls_variedad, ls_planta, ls_calibre, ls_predio, ls_cuartel
Date		ld_fecemb

ls_folio 	=	istr_mant.argumento[5]
ls_cliente	=	istr_Mant.Argumento[1]
ls_planta	=	istr_Mant.Argumento[2]

FOR ll_fil = 1 TO dw_2.RowCount()
	dw_2.Object.caja_traspa[ll_fil] = 0
NEXT	

FOR ll_fila1 = 1 TO dw_1.RowCount()
	FOR ll_fila2 = dw_1.Object.caja_traspa[ll_fila1] TO 1 step -1
		ls_variedad = string(dw_1.Object.pafr_varrot[ll_fila1])
		ls_embalaje = string(dw_1.Object.emba_codigo[ll_fila1])
		ls_productor = string(dw_1.Object.pafr_prdrot[ll_fila1])		
		ls_calibre 	= string(dw_1.Object.pafr_calrot[ll_fila1])		
		ld_fecemb 	= Date(dw_1.Object.pafr_fecemb[ll_fila1])	
		ls_cuartel 	= string(dw_1.Object.pafr_cuart1[ll_fila1])		
		ls_predio 	= String(dw_1.Object.pafr_huert1[ll_fila1])	
				
		ll_fila	= dw_2.Find( "clie_codigo = " + ls_cliente + &
								 " AND paen_numero = " + ls_folio + &
								 " AND pafr_varrot = " + ls_variedad + &
								 " AND emba_codigo = '" + ls_embalaje + "'" + &
								 " AND pafr_prdrot = " + ls_productor + &
								 " AND plde_codigo = " + ls_planta + &
								 " AND pafr_huert1 = " + ls_predio + &
								 " AND pafr_cuart1 = " + ls_cuartel + &
								 " AND caja_traspa = " + '0' + &
								 " AND pafr_ccajas = " + '1' + &
								 " AND String(pafr_fecemb,'dd-mm-yyyy')	=	'" + String(ld_fecemb, "dd-mm-yyyy") + "'" + &
								 " AND pafr_calrot = '" + ls_calibre + "'", 1, dw_2.RowCount())
								 
	IF ll_fila > 0 THEN dw_2.Object.caja_traspa[ll_fila] = 1
	
	NEXT
NEXT

MessageBox("Atención", "Cajas Aplicadas a Repalletizaje.", Exclamation!, OK!)

end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_repa_agrupado
integer x = 3863
integer y = 620
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_repa_agrupado
integer x = 9
integer y = 96
integer width = 3735
integer height = 1592
string dataobject = "dw_mues_palletfruta_repaletizaje_origen_caja"
boolean hscrollbar = true
boolean vscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_nula

SetNull(ls_Nula)

ls_columna = dwo.name

CHOOSE CASE ls_columna		
	CASE "caja_traspa"
		IF dw_1.Object.cajas[row] < Long(data) THEN
			MessageBox("Atención", "Cajas a Traspasar NO Puede ser Mayor a Cajas Disponibles.", Exclamation!, OK!)
			This.SetItem(Row, ls_Columna, Long(ls_Nula))
			Return 1
		END IF	
END CHOOSE
end event

event dw_1::clicked;call super::clicked;String	ls_columna

end event

type dw_2 from datawindow within w_mant_deta_repa_agrupado
boolean visible = false
integer x = 174
integer y = 1864
integer width = 2391
integer height = 672
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_palletfruta_repaletizaje_origen"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_mant_deta_repa_agrupado
boolean visible = false
integer x = 1385
integer y = 2012
integer width = 2176
integer height = 668
integer taborder = 50
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_palletfruta_repaletizaje_origen"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

