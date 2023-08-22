$PBExportHeader$w_info_cuadratura_comer.srw
$PBExportComments$Informe de Existencia de Fruta Comercial en Camara por Productor y Calibre.
forward
global type w_info_cuadratura_comer from w_para_informes
end type
type st_1 from statictext within w_info_cuadratura_comer
end type
type uo_selprod from uo_seleccion_productor within w_info_cuadratura_comer
end type
type st_2 from statictext within w_info_cuadratura_comer
end type
type st_3 from statictext within w_info_cuadratura_comer
end type
type st_4 from statictext within w_info_cuadratura_comer
end type
type uo_selplta from uo_seleccion_plantas within w_info_cuadratura_comer
end type
type st_6 from statictext within w_info_cuadratura_comer
end type
type st_12 from statictext within w_info_cuadratura_comer
end type
type uo_selespe from uo_seleccion_especie within w_info_cuadratura_comer
end type
type st_11 from statictext within w_info_cuadratura_comer
end type
type uo_selexpo from uo_seleccion_clientesprod within w_info_cuadratura_comer
end type
type sle_proc from singlelineedit within w_info_cuadratura_comer
end type
type cb_1 from commandbutton within w_info_cuadratura_comer
end type
type ddlb_proc from dropdownlistbox within w_info_cuadratura_comer
end type
type st_8 from statictext within w_info_cuadratura_comer
end type
type st_9 from statictext within w_info_cuadratura_comer
end type
type st_10 from statictext within w_info_cuadratura_comer
end type
type cbx_todos from checkbox within w_info_cuadratura_comer
end type
type cbx_2 from checkbox within w_info_cuadratura_comer
end type
type sle_lote from singlelineedit within w_info_cuadratura_comer
end type
type st_5 from statictext within w_info_cuadratura_comer
end type
type cbx_3 from checkbox within w_info_cuadratura_comer
end type
type cbx_4 from checkbox within w_info_cuadratura_comer
end type
end forward

global type w_info_cuadratura_comer from w_para_informes
integer x = 0
integer y = 0
integer width = 3456
integer height = 1432
string title = "INFORME DE CUADRATURA"
st_1 st_1
uo_selprod uo_selprod
st_2 st_2
st_3 st_3
st_4 st_4
uo_selplta uo_selplta
st_6 st_6
st_12 st_12
uo_selespe uo_selespe
st_11 st_11
uo_selexpo uo_selexpo
sle_proc sle_proc
cb_1 cb_1
ddlb_proc ddlb_proc
st_8 st_8
st_9 st_9
st_10 st_10
cbx_todos cbx_todos
cbx_2 cbx_2
sle_lote sle_lote
st_5 st_5
cbx_3 cbx_3
cbx_4 cbx_4
end type
global w_info_cuadratura_comer w_info_cuadratura_comer

type variables
Integer ii_tipo, ii_nroproc, ii_planta, ii_lote
uo_spro_lotesfrutacomenc		iuo_spro_lotesfrutacomenc
end variables

forward prototypes
public subroutine consolidalote ()
public function boolean existeorden ()
public subroutine existeinterdocto ()
public subroutine existelote (long al_numero)
end prototypes

public subroutine consolidalote ();
end subroutine

public function boolean existeorden (); long ll_count
 integer li_lote
 
  SELECT count()
    INTO :ll_count 
    FROM dba.spro_movtofrutacomenca  
   WHERE :ii_planta in (-1,plde_codigo) AND  
         :ii_nroproc in (-1,-9,mfco_docrel) AND
			:ii_tipo in (-1,-9,mfco_tipdoc);

IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla spro_ordenproceso")
		Return false
ELSEIF ll_count = 0 THEN
	messagebox("Error de Consistencia","No existe numero de proceso para el tipo")
	Return false
ELSE	
	Return True
END IF


  
	

end function

public subroutine existeinterdocto ();long 		ll_count


SELECT count() 
 INTO :ll_count  
 FROM dba.spro_doctointernopack  
WHERE :ii_planta in (-1,plde_codigo) 
AND  :ii_nroproc in (-1,-9,dinp_numero)
and  :ii_tipo in (-1,-9,dinp_tipdoc);


IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_doctointernopack")

ELSEIF ll_count = 0 THEN
	messagebox("Error de Consistencia","No existe numero de proceso para el tipo")
	
END IF

end subroutine

public subroutine existelote (long al_numero);Long		ll_cont
Integer	li_especie

li_especie = uo_SelEspe.Codigo

SELECT count()
INTO :ll_cont
FROM dba.spro_lotesfrutacomenc
WHERE :ii_planta in (-1,lofc_pltcod)  AND
	:li_especie in (-1,lofc_espcod)  AND
	:al_numero in (-1,-9,lofc_lotefc);
	
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_lotesfrutacomenc")

ELSEIF ll_cont = 0 THEN
	messagebox("Error de Consistencia","No existe Nº Lote")
	sle_lote.Text = ''
	sle_lote.SetFocus()
	
END IF	
	
end subroutine

on w_info_cuadratura_comer.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_selprod=create uo_selprod
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
this.uo_selplta=create uo_selplta
this.st_6=create st_6
this.st_12=create st_12
this.uo_selespe=create uo_selespe
this.st_11=create st_11
this.uo_selexpo=create uo_selexpo
this.sle_proc=create sle_proc
this.cb_1=create cb_1
this.ddlb_proc=create ddlb_proc
this.st_8=create st_8
this.st_9=create st_9
this.st_10=create st_10
this.cbx_todos=create cbx_todos
this.cbx_2=create cbx_2
this.sle_lote=create sle_lote
this.st_5=create st_5
this.cbx_3=create cbx_3
this.cbx_4=create cbx_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selprod
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.uo_selplta
this.Control[iCurrent+7]=this.st_6
this.Control[iCurrent+8]=this.st_12
this.Control[iCurrent+9]=this.uo_selespe
this.Control[iCurrent+10]=this.st_11
this.Control[iCurrent+11]=this.uo_selexpo
this.Control[iCurrent+12]=this.sle_proc
this.Control[iCurrent+13]=this.cb_1
this.Control[iCurrent+14]=this.ddlb_proc
this.Control[iCurrent+15]=this.st_8
this.Control[iCurrent+16]=this.st_9
this.Control[iCurrent+17]=this.st_10
this.Control[iCurrent+18]=this.cbx_todos
this.Control[iCurrent+19]=this.cbx_2
this.Control[iCurrent+20]=this.sle_lote
this.Control[iCurrent+21]=this.st_5
this.Control[iCurrent+22]=this.cbx_3
this.Control[iCurrent+23]=this.cbx_4
end on

on w_info_cuadratura_comer.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selprod)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.uo_selplta)
destroy(this.st_6)
destroy(this.st_12)
destroy(this.uo_selespe)
destroy(this.st_11)
destroy(this.uo_selexpo)
destroy(this.sle_proc)
destroy(this.cb_1)
destroy(this.ddlb_proc)
destroy(this.st_8)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.cbx_todos)
destroy(this.cbx_2)
destroy(this.sle_lote)
destroy(this.st_5)
destroy(this.cbx_3)
destroy(this.cbx_4)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelExpo.codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelPlta.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelProd.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_SelEspe.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelExpo.Seleccion(False, False)
	uo_SelEspe.Seleccion(True, False)
	uo_SelProd.Seleccion(True,False)
	uo_SelPlta.Seleccion(True,False)
	uo_SelProd.Todos(True)
	
	uo_SelExpo.Inicia(gi_codexport)
	uo_SelProd.Filtra(-1)
	iuo_spro_lotesfrutacomenc				=	Create uo_spro_lotesfrutacomenc

	ii_tipo 		= -1
	ii_nroproc	= -1
	ii_lote 		= -1
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_cuadratura_comer
integer x = 3099
integer y = 248
end type

type st_computador from w_para_informes`st_computador within w_info_cuadratura_comer
integer x = 1006
integer width = 2405
end type

type st_usuario from w_para_informes`st_usuario within w_info_cuadratura_comer
integer x = 1006
integer width = 2405
end type

type st_temporada from w_para_informes`st_temporada within w_info_cuadratura_comer
integer x = 1006
integer width = 2405
end type

type p_logo from w_para_informes`p_logo within w_info_cuadratura_comer
end type

type st_titulo from w_para_informes`st_titulo within w_info_cuadratura_comer
integer width = 2770
string text = "Informe Cuadratura Comercial"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_cuadratura_comer
integer x = 3099
integer y = 588
integer taborder = 120
integer textsize = -8
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila


istr_info.titulo	= "CUADRATURA COMERCIAL"
istr_info.copias	= 1
	
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_cuadratura_comercial"
vinf.dw_1.SetTransObject(sqlca)
ll_Fila	=	vinf.dw_1.Retrieve(uo_SelExpo.Codigo, uo_SelPlta.Codigo, uo_SelEspe.Codigo,&
									uo_SelProd.Codigo, ii_lote, ii_tipo, ii_nroproc)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Object.DataWindow.Zoom = 90	
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_cuadratura_comer
integer x = 3104
integer y = 888
integer taborder = 140
integer textsize = -8
fontcharset fontcharset = ansi!
end type

type st_1 from statictext within w_info_cuadratura_comer
integer x = 251
integer y = 416
integer width = 1394
integer height = 432
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selprod from uo_seleccion_productor within w_info_cuadratura_comer
integer x = 672
integer y = 628
integer taborder = 10
boolean bringtotop = true
end type

on uo_selprod.destroy
call uo_seleccion_productor::destroy
end on

type st_2 from statictext within w_info_cuadratura_comer
integer x = 315
integer y = 720
integer width = 338
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_cuadratura_comer
integer x = 315
integer y = 524
integer width = 338
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Exportador"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_cuadratura_comer
integer x = 1705
integer y = 524
integer width = 320
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type uo_selplta from uo_seleccion_plantas within w_info_cuadratura_comer
integer x = 2071
integer y = 628
integer taborder = 20
boolean bringtotop = true
end type

on uo_selplta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
	//	uo_SelCama.Todos(True)
		
		//uo_SelCama.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		//uo_SelCama.Filtra(This.Codigo)
		
		//uo_SelCama.cbx_Todos.Enabled	=	True
		
END CHOOSE

ConsolidaLote()
end event

type st_6 from statictext within w_info_cuadratura_comer
integer x = 1705
integer y = 720
integer width = 274
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_cuadratura_comer
integer x = 1646
integer y = 848
integer width = 1376
integer height = 380
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespe from uo_seleccion_especie within w_info_cuadratura_comer
integer x = 2071
integer y = 432
integer height = 188
integer taborder = 40
end type

on uo_selespe.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
//		uo_SelVari.Todos(True)
		
	//	uo_SelVari.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		//uo_SelVari.Filtra(This.Codigo)
		
	//	uo_SelVari.cbx_Todos.Enabled	=	True
		
END CHOOSE

ConsolidaLote()


end event

type st_11 from statictext within w_info_cuadratura_comer
integer x = 1646
integer y = 416
integer width = 1376
integer height = 432
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selexpo from uo_seleccion_clientesprod within w_info_cuadratura_comer
integer x = 672
integer y = 508
integer height = 92
integer taborder = 60
boolean bringtotop = true
end type

on uo_selexpo.destroy
call uo_seleccion_clientesprod::destroy
end on

type sle_proc from singlelineedit within w_info_cuadratura_comer
integer x = 677
integer y = 1064
integer width = 334
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
boolean righttoleft = true
end type

event modified;ii_nroproc = integer(sle_proc.text)

IF ii_tipo < 1 then
	messagebox("Error de Consistencia","Debe ingresa primero tipo de proceso")
	return 1
ELSE		
	ii_planta = uo_SelPlta.Codigo
	existeorden()
end if

end event

type cb_1 from commandbutton within w_info_cuadratura_comer
boolean visible = false
integer x = 1097
integer y = 1352
integer width = 96
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;str_busqueda	lstr_busq

lstr_busq.argum[6]= ddlb_proc.text

IF ddlb_proc.text = "Re - Proceso" THEN
	
	OpenWithParm(w_busqueda_doctointernopack,lstr_busq)
	lstr_busq	= Message.PowerObjectParm

	if lstr_busq.argum[1] <> "" then
//		dw_planta.object.plde_codigo[1] = integer(lstr_busq.argum[1])
		if lstr_busq.argum[2] = "4" then
			ddlb_proc.text = "Proceso"
		elseif lstr_busq.argum[2] = "5" then
			ddlb_proc.text = "Re - Proceso"
		end if	
		sle_proc.text = lstr_busq.argum[3]
//		dw_especie.object.espe_codigo[1] = integer(lstr_busq.argum[4])
//		em_fecha.text = mid(lstr_busq.argum[5],1,10)
		ii_tipo = integer( lstr_busq.argum[2] )
		ii_planta = integer(lstr_busq.argum[1])
		ii_nroproc = integer(lstr_busq.argum[3])
//		ii_especie = integer(lstr_busq.argum[4])
	
	end if 	
ELSEIF  ddlb_proc.text = "Proceso" THEN	
	OpenWithParm(w_busqueda_ordenproceso,lstr_busq)
	lstr_busq	= Message.PowerObjectParm

	if lstr_busq.argum[1] <> "" then
//		dw_planta.object.plde_codigo[1] = integer(lstr_busq.argum[1])
		if lstr_busq.argum[2] = "4" then
			ddlb_proc.text = "Proceso"
		elseif lstr_busq.argum[2] = "5" then
			ddlb_proc.text = "Re - Proceso"
		end if	
		sle_proc.text = lstr_busq.argum[3]
//		dw_especie.object.espe_codigo[1] = integer(lstr_busq.argum[4])
//		em_fecha.text = mid(lstr_busq.argum[5],1,10)
		ii_tipo = integer( lstr_busq.argum[2] )
		ii_planta = integer(lstr_busq.argum[1])
		ii_nroproc = integer(lstr_busq.argum[3])
//		ii_especie = integer(lstr_busq.argum[4])
	
	end if 	
END IF

end event

type ddlb_proc from dropdownlistbox within w_info_cuadratura_comer
integer x = 681
integer y = 924
integer width = 539
integer height = 452
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string item[] = {"1. - Proceso","2. - Re Proceso","3. - Re Embalaje","4. - Pre Proceso"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE Index
		
	CASE 2
		ii_Tipo	=	5
		
	CASE 3
		ii_Tipo	=	7
		
	CASE 4
		ii_Tipo	=	8
		
	CASE ELSE
		ii_Tipo	=	4
		
END CHOOSE

//sle_mensa.text			= 	""
end event

type st_8 from statictext within w_info_cuadratura_comer
integer x = 315
integer y = 948
integer width = 352
integer height = 92
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo de Proc."
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_cuadratura_comer
integer x = 315
integer y = 1084
integer width = 352
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro. Proceso"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_cuadratura_comer
integer x = 251
integer y = 848
integer width = 1394
integer height = 380
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_todos from checkbox within w_info_cuadratura_comer
integer x = 1230
integer y = 940
integer width = 389
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	sle_proc.Text = ''
	ii_nroproc = -1
	ii_tipo	= -1	
	ddlb_proc.Enabled = False
	sle_proc.Enabled  = False
ELSE
	ii_nroproc = Integer(sle_proc.Text)
	ddlb_proc.Enabled = True
	sle_proc.Enabled = True
END IF	
end event

type cbx_2 from checkbox within w_info_cuadratura_comer
integer x = 1230
integer y = 1064
integer width = 389
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
end type

event clicked;IF This.Checked THEN
	sle_proc.Text = ''
	ii_nroproc = -9
	ii_tipo = -9
	ddlb_proc.Enabled = False
	sle_proc.Enabled  = False
	cbx_todos.Checked = True
	cbx_todos.Enabled = False
ELSE
	ii_nroproc = -1
	ii_tipo = -1
	cbx_todos.Enabled = True
	cbx_todos.Checked = True
END IF	
end event

type sle_lote from singlelineedit within w_info_cuadratura_comer
integer x = 2071
integer y = 1044
integer width = 466
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
boolean righttoleft = true
end type

event modified;ii_planta = uo_SelPlta.Codigo

IF iuo_spro_lotesfrutacomenc.Existe(ii_planta,uo_SelEspe.Codigo,Long(This.Text),True,Sqlca) THEN
	sle_lote.Text = ''
ELSE
	ii_lote = Long(sle_lote.Text)
END IF	

end event

type st_5 from statictext within w_info_cuadratura_comer
integer x = 1705
integer y = 1064
integer width = 274
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Lote"
boolean focusrectangle = false
end type

type cbx_3 from checkbox within w_info_cuadratura_comer
integer x = 2080
integer y = 948
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	ii_lote = -1
	sle_lote.Text = ''
	sle_lote.Enabled = False
ELSE
	sle_lote.Enabled = True
END IF	
	
end event

type cbx_4 from checkbox within w_info_cuadratura_comer
integer x = 2455
integer y = 948
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
end type

event clicked;IF This.Checked THEN
	ii_lote = -9
	sle_lote.Text = ''
	sle_lote.Enabled = False
	cbx_3.Checked = True
	cbx_3.Enabled = False
ELSE
	sle_lote.Enabled = True
	cbx_3.Checked = True
	cbx_3.Enabled = True
	ii_lote = -1
END IF	
	
end event

