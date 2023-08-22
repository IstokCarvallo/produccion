$PBExportHeader$w_mant_deta_movtoserviadideta.srw
forward
global type w_mant_deta_movtoserviadideta from w_mant_detalle_csd
end type
type dw_2 from datawindow within w_mant_deta_movtoserviadideta
end type
type dw_3 from datawindow within w_mant_deta_movtoserviadideta
end type
type dw_4 from datawindow within w_mant_deta_movtoserviadideta
end type
end forward

global type w_mant_deta_movtoserviadideta from w_mant_detalle_csd
integer width = 4983
integer height = 2104
dw_2 dw_2
dw_3 dw_3
dw_4 dw_4
end type
global w_mant_deta_movtoserviadideta w_mant_deta_movtoserviadideta

type variables
Integer ii_estado, ii_filanew
end variables

forward prototypes
public subroutine buscapallet ()
public function boolean duplicado (long al_numero)
public function boolean noexistepallet (long al_numero)
public subroutine wf_nuevo ()
end prototypes

public subroutine buscapallet ();dw_1.Modify("buscapallet.border = 0")
dw_1.Modify("buscapallet.border = 5")

istr_busq.argum[2]	=	""
istr_busq.argum[1]	=	String(dw_1.Object.clie_codigo[il_fila])
istr_busq.argum[12]	=	String(dw_1.Object.plde_codigo[il_fila])
istr_busq.argum[3]	=	"1"

OpenWithParm(w_busc_palletencab_busc, istr_busq)

istr_busq	       	=	Message.PowerObjectParm

IF istr_busq.argum[2] <> "" THEN
	IF Duplicado(Long(istr_busq.Argum[2])) THEN
		dw_1.SetFocus()
		Return
	ELSE
		dw_1.SetColumn("mosd_observ")
		dw_1.SetFocus()
	END IF
	
	NoExistePallet(Long(istr_busq.Argum[2]))
ELSE
	dw_1.SetColumn("paen_numero")
	dw_1.SetFocus()
END IF

dw_1.Modify("buscapallet.border = 0")
dw_1.Modify("buscapallet.border = 6")

RETURN
end subroutine

public function boolean duplicado (long al_numero);Long		ll_fila
Integer	li_cliente, li_planta, li_cantid

ll_fila	=	dw_1.Find("clie_codigo = " + istr_mant.Argumento[2] + &
						" AND plde_codigo = " + istr_mant.Argumento[1] + &
						" AND paen_numero = " + String(al_Numero) , 1, dw_1.RowCount())
	
IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Pallet ya fue incluido en Detalle", Information!, Ok!)
	RETURN True
END IF

RETURN False
end function

public function boolean noexistepallet (long al_numero);String	ls_nomvar, ls_embala
Integer	li_catego, li_cliente, li_ContCalidad, li_tipopa, li_cajas, li_inspec, &
			li_Destino, li_Especie, li_Variedad, li_planta, li_condicion, li_estadofumi, li_respuesta
Long		ll_fumigacion, ll_fila, ll_fil, ll_new
Date		ld_fecfumi

li_cliente	= 	Integer(istr_mant.argumento[2])
li_planta	=	Integer(istr_mant.argumento[1])

SELECT	pae.paen_tipopa, var.vari_nombre, pae.emba_codigo, pae.cate_codigo,
			pae.paen_concal, pae.paen_ccajas, pae.paen_inspec, pae.dest_codigo,
			pae.espe_codigo, pae.vari_codigo, pae.paen_estado, pae.cond_codigo,
			pae.fumi_numero, pae.fumi_fecfum
	INTO	:li_tipopa, :ls_nomvar, :ls_embala, :li_catego, :li_ContCalidad, :li_cajas,
			:li_inspec, :li_Destino, :li_Especie, :li_Variedad, :ii_estado, :li_condicion,
			:ll_fumigacion,:ld_fecfumi
	FROM	dba.palletencab as pae, dba.variedades as var
	WHERE pae.clie_codigo	= 	:li_cliente
	AND	pae.paen_numero	= 	:al_numero
	AND	pae.plde_codigo	=	:li_planta
	AND	var.espe_codigo	= 	pae.espe_codigo
	AND	var.vari_codigo	= 	pae.vari_codigo
	AND   exists(select *
	from dba.palletfruta as paf
	where paf.clie_codigo=pae.clie_codigo
	and   paf.plde_codigo=pae.plde_codigo
	and   paf.paen_numero=pae.paen_numero);

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletencab")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Número de Pallet no ha sido creado, Ingrese otro Código.", &
			Exclamation!, OK!)
	RETURN True
END IF

IF ii_estado = 2 THEN
	MessageBox("Atención", "Pallet ya fue Despachado desde Planta Despachadora.", &
			Exclamation!, OK!)
	RETURN True
ELSEIF ii_estado = 3 THEN
	MessageBox("Atención", "Pallet fue Repalletizado en Planta Despachadora.", &
			Exclamation!, OK!)
	RETURN True
END IF

dw_3.SetTransObject(Sqlca)

ll_fila = dw_3.Retrieve(li_cliente,li_planta,al_numero)

IF ll_fila > 0 THEN
	FOR ll_fil = 1 TO ll_fila
		ll_new = dw_2.InsertRow(0)
		
		dw_2.Object.plde_codigo[ll_new] = dw_1.Object.plde_codigo[il_fila]
		dw_2.Object.clie_codigo[ll_new] = dw_1.Object.clie_codigo[il_fila] 
		dw_2.Object.sere_codigo[ll_new] = dw_1.Object.sere_codigo[il_fila] 
		dw_2.Object.serd_codigo[ll_new] = dw_1.Object.serd_codigo[il_fila]  
		dw_2.Object.paen_numero[ll_new] = al_numero  
		dw_2.Object.prod_nombre[ll_new] = dw_3.Object.prod_nombre[ll_fil]  
		dw_2.Object.prod_codigo[ll_new] = dw_3.Object.prod_codigo[ll_fil]  
		dw_2.Object.espe_codigo[ll_new] = dw_3.Object.espe_codigo[ll_fil]   
		dw_2.Object.vari_codigo[ll_new] = dw_3.Object.vari_codigo[ll_fil] 
		dw_2.Object.vari_nombre[ll_new] = dw_3.Object.vari_nombre[ll_fil]
		dw_2.Object.emba_codigo[ll_new] = dw_3.Object.emba_codigo[ll_fil]   
		dw_2.Object.cond_codigo[ll_new] = dw_3.Object.cond_codigo[ll_fil] 
		dw_2.Object.cond_nombre[ll_new] = dw_3.Object.cond_nombre[ll_fil] 
		dw_2.Object.etiq_codigo[ll_new] = dw_3.Object.etiq_codigo[ll_fil]   
		dw_2.Object.etiq_nombre[ll_new] = dw_3.Object.etiq_nombre[ll_fil]   
		dw_2.Object.pafr_calibr[ll_new] = dw_3.Object.pafr_calibr[ll_fil]   
		dw_2.Object.pafr_copack[ll_new] = dw_3.Object.pafr_copack[ll_fil]   
		dw_2.Object.plde_nombre[ll_new] = dw_3.Object.plde_nombre[ll_fil]   
		dw_2.Object.cate_codigo[ll_new] = dw_3.Object.cate_codigo[ll_fil]   
		dw_2.Object.cate_nombre[ll_new] = dw_3.Object.cate_nombre[ll_fil]   
		dw_2.Object.pafr_ccajas[ll_new] = dw_3.Object.pafr_ccajas[ll_fil]   
	NEXT
END IF
dw_1.SetItem(il_fila, "paen_numero", al_numero)
dw_1.SetItem(il_fila, "espe_codigo", li_Especie)
//dw_1.SetItem(il_fila, "vari_codigo", li_Variedad)
dw_1.SetItem(il_fila, "vari_nombre", ls_nomvar)
//dw_1.SetItem(il_fila, "paen_tipopa", li_tipopa)
//dw_1.SetItem(il_fila, "emba_codigo", ls_embala)
//dw_1.SetItem(il_fila, "cate_codigo", li_catego)
dw_1.SetItem(il_fila, "paen_ccajas", li_cajas)
dw_1.SetColumn("mosd_observ")
dw_1.SetFocus()

RETURN False
end function

public subroutine wf_nuevo ();il_fila 		= dw_1.InsertRow(0)
ii_filanew	= dw_3.InsertRow(0)

dw_1.SetRedraw(False)
dw_1.SetRow(il_fila)
dw_1.ScrollToRow(il_fila)
istr_mant.dw.SetRow(il_fila)
istr_mant.dw.ScrolltoRow(il_fila)
istr_mant.dw.SelectRow(0,False)
istr_mant.dw.SelectRow(il_fila,True)
dw_1.SetRedraw(True)
end subroutine

on w_mant_deta_movtoserviadideta.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.dw_3=create dw_3
this.dw_4=create dw_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.dw_3
this.Control[iCurrent+3]=this.dw_4
end on

on w_mant_deta_movtoserviadideta.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.dw_3)
destroy(this.dw_4)
end on

event ue_recuperadatos;call super::ue_recuperadatos;dw_1.modify("buscapallet.Visible=1")

ias_campo[1]	=	String(dw_1.Object.paen_numero[il_fila])

IF istr_mant.agrega THEN
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[2]))
	dw_1.SetItem(il_fila, "mose_numero", Long(istr_mant.argumento[3]))
	dw_1.SetItem(il_fila, "sere_codigo", Integer(istr_mant.argumento[4]))
	dw_1.SetItem(il_fila, "serd_codigo", Integer(istr_mant.argumento[5]))
END IF

IF istr_mant.agrega = False and istr_mant.borra = False THEN
	dw_2.Retrieve(Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[1]),dw_1.Object.paen_numero[il_fila])
//	dw_4.Retrieve(Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[1]),dw_1.Object.paen_numero[il_fila])
//	dw_3.Retrieve(Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[1]),dw_1.Object.paen_numero[il_fila])
	dw_1.SetTabOrder("paen_numero", 0)
	dw_1.Modify("paen_numero.BackGround.Color = " + String(RGB(166,180,210)))
	dw_1.modify("buscapallet.Visible=0")
END IF

end event

event ue_deshace;call super::ue_deshace;dw_1.Object.paen_numero[il_fila]	=	Long(ias_campo[1])

end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.paen_numero[il_fila]) OR dw_1.Object.paen_numero[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nNúmero de Pallet"
	ls_colu[li_cont]	= "paen_numero"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus() 
	Message.DoubleParm = -1
END IF
end event

event ue_nuevo;Long	ll_fila, ll_new, ll_fil
ib_ok = True
Long		ll_cajas

This.TriggerEvent("ue_guardar")
IF Message.DoubleParm = -1 THEN ib_ok = False

IF ib_ok = False THEN RETURN

FOR ll_fil = 1 TO dw_2.RowCount()
	ll_new = dw_4.InsertRow(0)
	
	dw_4.Object.plde_codigo[ll_new] = dw_1.Object.plde_codigo[il_fila]
	dw_4.Object.clie_codigo[ll_new] = dw_1.Object.clie_codigo[il_fila] 
	dw_4.Object.sere_codigo[ll_new] = dw_1.Object.sere_codigo[il_fila] 
	dw_4.Object.serd_codigo[ll_new] = dw_1.Object.serd_codigo[il_fila]  
	dw_4.Object.paen_numero[ll_new] = dw_1.Object.paen_numero[il_fila]  
	dw_4.Object.prod_nombre[ll_new] = dw_2.Object.prod_nombre[ll_fil]  
	dw_4.Object.prod_codigo[ll_new] = dw_2.Object.prod_codigo[ll_fil]  
	dw_4.Object.espe_codigo[ll_new] = dw_2.Object.espe_codigo[ll_fil]   
	dw_4.Object.vari_codigo[ll_new] = dw_2.Object.vari_codigo[ll_fil] 
	dw_4.Object.vari_nombre[ll_new] = dw_2.Object.vari_nombre[ll_fil]
	dw_4.Object.emba_codigo[ll_new] = dw_2.Object.emba_codigo[ll_fil]   
	dw_4.Object.cond_codigo[ll_new] = dw_2.Object.cond_codigo[ll_fil] 
	dw_4.Object.cond_nombre[ll_new] = dw_2.Object.cond_nombre[ll_fil] 
	dw_4.Object.etiq_codigo[ll_new] = dw_2.Object.etiq_codigo[ll_fil]   
	dw_4.Object.etiq_nombre[ll_new] = dw_2.Object.etiq_nombre[ll_fil]   
	dw_4.Object.pafr_calibr[ll_new] = dw_2.Object.pafr_calibr[ll_fil]   
	dw_4.Object.pafr_copack[ll_new] = dw_2.Object.pafr_copack[ll_fil]   
	dw_4.Object.plde_nombre[ll_new] = dw_2.Object.plde_nombre[ll_fil]   
	dw_4.Object.cate_codigo[ll_new] = dw_2.Object.cate_codigo[ll_fil]   
	dw_4.Object.cate_nombre[ll_new] = dw_2.Object.cate_nombre[ll_fil]   
	dw_4.Object.pafr_ccajas[ll_new] = dw_2.Object.pafr_ccajas[ll_fil]   
NEXT

ll_cajas	=	Long(dw_1.object.totcajas[1])

dw_2.Reset()

wf_nuevo()
dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "clie_codigo", Integer(istr_mant.argumento[2]))
dw_1.SetItem(il_fila, "mose_numero", Long(istr_mant.argumento[3]))
dw_1.SetItem(il_fila, "sere_codigo", Integer(istr_mant.argumento[4]))
dw_1.SetItem(il_fila, "serd_codigo", Integer(istr_mant.argumento[5]))

dw_1.SetColumn("paen_numero")
dw_1.SetFocus()

end event

event resize;//
end event

event open;call super::open;dw_4.SetTransObject(sqlca)
istr_mant.dw2.ShareData(dw_4)
dw_3.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_movtoserviadideta
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_movtoserviadideta
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_movtoserviadideta
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_movtoserviadideta
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_movtoserviadideta
integer x = 4713
integer y = 1252
end type

event pb_cancela::clicked;call super::clicked;IF dw_3.RowCount() > 0 THEN
	dw_3.Reset()
END IF	
end event

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_movtoserviadideta
integer x = 4709
integer y = 1036
end type

event pb_acepta::clicked;Long	ll_cajas, ll_fil, ll_new

istr_mant.respuesta = 1

IF istr_mant.agrega THEN
	
	Parent.TriggerEvent("ue_nuevo")
		
ELSE
	FOR ll_fil = 1 TO dw_2.RowCount()
		ll_new = dw_4.InsertRow(0)
		
		dw_4.Object.plde_codigo[ll_new] = dw_1.Object.plde_codigo[il_fila]
		dw_4.Object.clie_codigo[ll_new] = dw_1.Object.clie_codigo[il_fila] 
		dw_4.Object.sere_codigo[ll_new] = dw_1.Object.sere_codigo[il_fila] 
		dw_4.Object.serd_codigo[ll_new] = dw_1.Object.serd_codigo[il_fila]  
		dw_4.Object.paen_numero[ll_new] = dw_1.Object.paen_numero[il_fila]  
		dw_4.Object.prod_nombre[ll_new] = dw_2.Object.prod_nombre[ll_fil]  
		dw_4.Object.prod_codigo[ll_new] = dw_2.Object.prod_codigo[ll_fil]  
		dw_4.Object.espe_codigo[ll_new] = dw_2.Object.espe_codigo[ll_fil]   
		dw_4.Object.vari_codigo[ll_new] = dw_2.Object.vari_codigo[ll_fil] 
		dw_4.Object.vari_nombre[ll_new] = dw_2.Object.vari_nombre[ll_fil]
		dw_4.Object.emba_codigo[ll_new] = dw_2.Object.emba_codigo[ll_fil]   
		dw_4.Object.cond_codigo[ll_new] = dw_2.Object.cond_codigo[ll_fil] 
		dw_4.Object.cond_nombre[ll_new] = dw_2.Object.cond_nombre[ll_fil] 
		dw_4.Object.etiq_codigo[ll_new] = dw_2.Object.etiq_codigo[ll_fil]   
		dw_4.Object.etiq_nombre[ll_new] = dw_2.Object.etiq_nombre[ll_fil]   
		dw_4.Object.pafr_calibr[ll_new] = dw_2.Object.pafr_calibr[ll_fil]   
		dw_4.Object.pafr_copack[ll_new] = dw_2.Object.pafr_copack[ll_fil]   
		dw_4.Object.plde_nombre[ll_new] = dw_2.Object.plde_nombre[ll_fil]   
		dw_4.Object.cate_codigo[ll_new] = dw_2.Object.cate_codigo[ll_fil]   
		dw_4.Object.cate_nombre[ll_new] = dw_2.Object.cate_nombre[ll_fil]   
		dw_4.Object.pafr_ccajas[ll_new] = dw_2.Object.pafr_ccajas[ll_fil]   
	NEXT
	CloseWithReturn(Parent, istr_mant)
END IF
end event

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_movtoserviadideta
integer x = 4713
integer y = 1468
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_movtoserviadideta
integer x = 878
integer y = 4
integer width = 2853
integer height = 732
string dataobject = "dw_mant_movtoseviadideta"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_Nula
Integer	li_cliente,pallet,li_status,li_tipo

SetNull(ls_Nula)

ls_columna = dwo.name

CHOOSE CASE ls_columna
			
	CASE "paen_numero"
		
		IF Duplicado(Long(data)) THEN
			dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
			RETURN 1
		END IF
		
		IF NoExistePallet(Long(data)) THEN
			dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
			RETURN 1
		END IF

END CHOOSE
end event

event dw_1::clicked;call super::clicked;String	ls_columna

ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "buscapallet"
		BuscaPallet()
		
END CHOOSE
end event

type dw_2 from datawindow within w_mant_deta_movtoserviadideta
integer y = 756
integer width = 4608
integer height = 1112
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_movtoservicioscaracteristicas"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;dw_3.Reset()
end event

type dw_3 from datawindow within w_mant_deta_movtoserviadideta
integer x = 96
integer y = 76
integer width = 741
integer height = 720
integer taborder = 20
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_palletmovtoservicioscaracteristicas"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from datawindow within w_mant_deta_movtoserviadideta
integer x = 3735
integer y = 40
integer width = 1175
integer height = 784
integer taborder = 30
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_mues_movtoservicioscaracteristicas"
boolean controlmenu = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

