$PBExportHeader$w_info_saldos_globales_por_tarjas.srw
forward
global type w_info_saldos_globales_por_tarjas from w_info_saldos_globales
end type
type rb_productor from radiobutton within w_info_saldos_globales_por_tarjas
end type
type rb_variedad from radiobutton within w_info_saldos_globales_por_tarjas
end type
type rb_embalaje from radiobutton within w_info_saldos_globales_por_tarjas
end type
type rb_categoria from radiobutton within w_info_saldos_globales_por_tarjas
end type
type rb_status from radiobutton within w_info_saldos_globales_por_tarjas
end type
type rb_etiqueta from radiobutton within w_info_saldos_globales_por_tarjas
end type
type rb_calibre from radiobutton within w_info_saldos_globales_por_tarjas
end type
end forward

global type w_info_saldos_globales_por_tarjas from w_info_saldos_globales
integer width = 2958
integer height = 1648
string title = "EXISTENCIAS GLOBALES POR TARJAS"
rb_productor rb_productor
rb_variedad rb_variedad
rb_embalaje rb_embalaje
rb_categoria rb_categoria
rb_status rb_status
rb_etiqueta rb_etiqueta
rb_calibre rb_calibre
end type
global w_info_saldos_globales_por_tarjas w_info_saldos_globales_por_tarjas

type variables
String is_NomPlanta, is_NomEspecie
Integer ii_Planta, ii_Cliente, ii_Productor,ii_Especie, ii_Variedad, ii_Status

DataWindowChild 	idwc_planta, idwc_Productor
end variables

on w_info_saldos_globales_por_tarjas.create
int iCurrent
call super::create
this.rb_productor=create rb_productor
this.rb_variedad=create rb_variedad
this.rb_embalaje=create rb_embalaje
this.rb_categoria=create rb_categoria
this.rb_status=create rb_status
this.rb_etiqueta=create rb_etiqueta
this.rb_calibre=create rb_calibre
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_productor
this.Control[iCurrent+2]=this.rb_variedad
this.Control[iCurrent+3]=this.rb_embalaje
this.Control[iCurrent+4]=this.rb_categoria
this.Control[iCurrent+5]=this.rb_status
this.Control[iCurrent+6]=this.rb_etiqueta
this.Control[iCurrent+7]=this.rb_calibre
end on

on w_info_saldos_globales_por_tarjas.destroy
call super::destroy
destroy(this.rb_productor)
destroy(this.rb_variedad)
destroy(this.rb_embalaje)
destroy(this.rb_categoria)
destroy(this.rb_status)
destroy(this.rb_etiqueta)
destroy(this.rb_calibre)
end on

event open;call super::open;dw_status.SetItem(1, "stat_codigo", 1)

ii_Cliente		=	gi_CodExport
ii_Especie		=	11
ii_Status		=	1
end event

type st_computador from w_info_saldos_globales`st_computador within w_info_saldos_globales_por_tarjas
end type

type st_usuario from w_info_saldos_globales`st_usuario within w_info_saldos_globales_por_tarjas
end type

type st_temporada from w_info_saldos_globales`st_temporada within w_info_saldos_globales_por_tarjas
end type

type p_logo from w_info_saldos_globales`p_logo within w_info_saldos_globales_por_tarjas
end type

type st_titulo from w_info_saldos_globales`st_titulo within w_info_saldos_globales_por_tarjas
string text = "Informe Existencias por Tarjas Fruta Procesada"
end type

type pb_acepta from w_info_saldos_globales`pb_acepta within w_info_saldos_globales_por_tarjas
integer x = 2615
integer y = 564
end type

event pb_acepta::clicked;Long		ll_Filas

istr_info.titulo	= 'INFORME DE SALDOS GLOBALES POR TARJA'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_despachos_globales_por_tarjas"
vinf.dw_1.SetTransObject(sqlca)

ll_Filas	=	vinf.dw_1.Retrieve(ii_Cliente, ii_Especie, ii_Status, ii_Variedad, &
										ii_Planta, ii_Productor)

IF rb_productor.Checked THEN
	vinf.dw_1.SetSort("palletfruta_prod_codigo")	
END IF

IF rb_variedad.Checked THEN
	vinf.dw_1.SetSort("vari_codigo")	
END IF

IF rb_embalaje.Checked THEN
	vinf.dw_1.SetSort("palletfruta_emba_codigo")	
END IF

IF rb_categoria.Checked THEN
	vinf.dw_1.SetSort("palletencab_cate_codigo")	
END IF

IF rb_status.Checked THEN
	vinf.dw_1.SetSort("palletencab_stat_codigo")	
END IF

IF rb_etiqueta.Checked THEN
	vinf.dw_1.SetSort("palletencab_etiq_codigo")	
END IF

IF rb_calibre.Checked THEN
	vinf.dw_1.SetSort("palletfruta_pafr_calibre")	
END IF

vinf.dw_1.Sort()


IF ll_Filas = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Filas = 0 THEN
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

type pb_salir from w_info_saldos_globales`pb_salir within w_info_saldos_globales_por_tarjas
integer x = 2606
end type

type dw_etiqueta from w_info_saldos_globales`dw_etiqueta within w_info_saldos_globales_por_tarjas
end type

type dw_status from w_info_saldos_globales`dw_status within w_info_saldos_globales_por_tarjas
integer y = 1336
end type

event dw_status::itemchanged;call super::itemchanged;ii_Status	=	Integer(data)
end event

type sle_vari_hasta from w_info_saldos_globales`sle_vari_hasta within w_info_saldos_globales_por_tarjas
end type

type sle_vari_desde from w_info_saldos_globales`sle_vari_desde within w_info_saldos_globales_por_tarjas
end type

type cb_1 from w_info_saldos_globales`cb_1 within w_info_saldos_globales_por_tarjas
end type

type cb_busc_var_d from w_info_saldos_globales`cb_busc_var_d within w_info_saldos_globales_por_tarjas
end type

type em_vari_hasta from w_info_saldos_globales`em_vari_hasta within w_info_saldos_globales_por_tarjas
end type

type em_vari_desde from w_info_saldos_globales`em_vari_desde within w_info_saldos_globales_por_tarjas
end type

type st_5 from w_info_saldos_globales`st_5 within w_info_saldos_globales_por_tarjas
integer y = 804
end type

type st_4 from w_info_saldos_globales`st_4 within w_info_saldos_globales_por_tarjas
end type

type st_3 from w_info_saldos_globales`st_3 within w_info_saldos_globales_por_tarjas
integer y = 1084
end type

type dw_especie from w_info_saldos_globales`dw_especie within w_info_saldos_globales_por_tarjas
integer y = 792
end type

event dw_especie::itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

IF ExisteEspecie(ii_Cliente,Integer(data), ls_Columna[]) THEN
	ii_Especie		=	Integer(data)
	is_NomEspecie	=	ls_Columna[1]
	idwc_variedades.Retrieve(ii_Cliente, ii_Especie)
ELSE
	This.SetItem(1, "espe_codigo", li_Nula)
	
	RETURN 1
END IF













			

end event

type st_2 from w_info_saldos_globales`st_2 within w_info_saldos_globales_por_tarjas
end type

type st_1 from w_info_saldos_globales`st_1 within w_info_saldos_globales_por_tarjas
end type

type dw_prod_hasta from w_info_saldos_globales`dw_prod_hasta within w_info_saldos_globales_por_tarjas
end type

type dw_prod_desde from w_info_saldos_globales`dw_prod_desde within w_info_saldos_globales_por_tarjas
integer y = 556
integer height = 100
end type

event dw_prod_desde::itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

IF ExisteProductor(ii_Cliente, Integer(data), ls_Columna[]) THEN
	ii_Productor	=	Integer(data)	
ELSE
	This.SetItem(1, "prod_codigo", li_Nula)
	
	RETURN 1
END IF
end event

type gb_5 from w_info_saldos_globales`gb_5 within w_info_saldos_globales_por_tarjas
integer x = 78
integer y = 712
end type

type gb_4 from w_info_saldos_globales`gb_4 within w_info_saldos_globales_por_tarjas
integer y = 412
end type

type gb_7 from w_info_saldos_globales`gb_7 within w_info_saldos_globales_por_tarjas
integer y = 1252
integer width = 1605
integer height = 248
end type

type gb_8 from w_info_saldos_globales`gb_8 within w_info_saldos_globales_por_tarjas
end type

type gb_6 from w_info_saldos_globales`gb_6 within w_info_saldos_globales_por_tarjas
integer y = 180
integer width = 1120
end type

type cbx_todas from w_info_saldos_globales`cbx_todas within w_info_saldos_globales_por_tarjas
integer x = 1783
integer y = 240
end type

event cbx_todas::clicked;call super::clicked;IF(This.Checked )THEN
	dw_planta.Enabled = False
	dw_planta.reset()
	dw_planta.insertrow(0)
ELSE
	dw_planta.Enabled = True
	dw_planta.setfocus()
END IF
end event

type dw_planta from w_info_saldos_globales`dw_planta within w_info_saldos_globales_por_tarjas
integer x = 1783
integer y = 324
end type

event dw_planta::itemchanged;call super::itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

IF ExistePlanta(ii_Cliente,Integer(data), ls_Columna[]) THEN
	ii_Planta		=	Integer(data)
	is_NomPlanta	=	ls_Columna[1]
ELSE
	This.SetItem(1, "plde_codigo", li_Nula)
	
	RETURN 1
END IF
end event

type gb_3 from w_info_saldos_globales`gb_3 within w_info_saldos_globales_por_tarjas
integer y = 476
integer height = 764
string text = "Ordenado Por"
end type

type cbx_calibre from w_info_saldos_globales`cbx_calibre within w_info_saldos_globales_por_tarjas
end type

type cbx_serie from w_info_saldos_globales`cbx_serie within w_info_saldos_globales_por_tarjas
boolean visible = false
integer y = 1072
end type

type cbx_etiqueta from w_info_saldos_globales`cbx_etiqueta within w_info_saldos_globales_por_tarjas
boolean visible = false
integer y = 984
end type

type cbx_status from w_info_saldos_globales`cbx_status within w_info_saldos_globales_por_tarjas
boolean visible = false
integer y = 896
end type

type cbx_categoria from w_info_saldos_globales`cbx_categoria within w_info_saldos_globales_por_tarjas
boolean visible = false
integer y = 808
end type

type cbx_envase from w_info_saldos_globales`cbx_envase within w_info_saldos_globales_por_tarjas
boolean visible = false
integer y = 720
end type

type cbx_variedad from w_info_saldos_globales`cbx_variedad within w_info_saldos_globales_por_tarjas
boolean visible = false
integer y = 632
end type

type cbx_productor from w_info_saldos_globales`cbx_productor within w_info_saldos_globales_por_tarjas
boolean visible = false
integer y = 544
end type

type cbx_prod from w_info_saldos_globales`cbx_prod within w_info_saldos_globales_por_tarjas
integer y = 468
end type

event cbx_prod::clicked;call super::clicked;IF This.Checked THEN
	dw_prod_desde.Enabled = False
	dw_prod_desde.reset()
	dw_prod_desde.insertrow(0)
ELSE
	dw_prod_desde.Enabled = True
	dw_prod_desde.SetFocus()
END IF
end event

type dw_vari from w_info_saldos_globales`dw_vari within w_info_saldos_globales_por_tarjas
integer y = 1068
end type

event dw_vari::itemchanged;call super::itemchanged;
String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

IF ExisteVariedad(ii_Cliente,ii_Especie,Integer(data), ls_Columna[]) THEN
	ii_Variedad		=	Integer(data)
	
ELSE
	This.SetItem(1, "vari_codigo", li_Nula)
	
	RETURN 1
END IF







end event

type cbx_vari from w_info_saldos_globales`cbx_vari within w_info_saldos_globales_por_tarjas
integer y = 952
end type

event cbx_vari::clicked;call super::clicked;IF(This.Checked )THEN
	dw_vari.Enabled = False
	dw_vari.reset()
	dw_vari.insertrow(0)
ELSE
	dw_vari.Enabled = True
	dw_vari.SetFocus()
END IF
end event

type dw_1 from w_info_saldos_globales`dw_1 within w_info_saldos_globales_por_tarjas
integer x = 302
integer y = 260
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_Columna[]
Integer	li_Nula

SetNull(li_Nula)

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	ii_Cliente		=	Integer(data)
	idwc_planta.retrieve(ii_Cliente)
ELSE
	This.SetItem(1, "clie_codigo", li_Nula)
	
	RETURN 1
END IF

end event

type gb_9 from w_info_saldos_globales`gb_9 within w_info_saldos_globales_por_tarjas
integer y = 180
end type

type rb_productor from radiobutton within w_info_saldos_globales_por_tarjas
integer x = 1806
integer y = 568
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Productor"
boolean checked = true
boolean lefttext = true
end type

type rb_variedad from radiobutton within w_info_saldos_globales_por_tarjas
integer x = 1806
integer y = 664
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Variedad"
boolean lefttext = true
end type

type rb_embalaje from radiobutton within w_info_saldos_globales_por_tarjas
integer x = 1806
integer y = 760
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Embalaje"
boolean lefttext = true
end type

type rb_categoria from radiobutton within w_info_saldos_globales_por_tarjas
integer x = 1806
integer y = 856
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Categoria"
boolean lefttext = true
end type

type rb_status from radiobutton within w_info_saldos_globales_por_tarjas
integer x = 1806
integer y = 952
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Status"
boolean lefttext = true
end type

type rb_etiqueta from radiobutton within w_info_saldos_globales_por_tarjas
integer x = 1801
integer y = 1048
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Etiqueta"
boolean lefttext = true
end type

type rb_calibre from radiobutton within w_info_saldos_globales_por_tarjas
integer x = 1806
integer y = 1148
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 78748035
string text = "Calibre"
boolean lefttext = true
end type

