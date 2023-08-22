$PBExportHeader$w_gene_proceso_valorizacion.srw
$PBExportComments$Genera la Valorización y Entrega Informe
forward
global type w_gene_proceso_valorizacion from window
end type
type cbx_conplan from checkbox within w_gene_proceso_valorizacion
end type
type cbx_conprod from checkbox within w_gene_proceso_valorizacion
end type
type cbx_archivo from checkbox within w_gene_proceso_valorizacion
end type
type dw_registro from datawindow within w_gene_proceso_valorizacion
end type
type em_porceniva from editmask within w_gene_proceso_valorizacion
end type
type st_4 from statictext within w_gene_proceso_valorizacion
end type
type cbx_todasplan from checkbox within w_gene_proceso_valorizacion
end type
type rb_ambos from radiobutton within w_gene_proceso_valorizacion
end type
type rb_salida from radiobutton within w_gene_proceso_valorizacion
end type
type rb_ingreso from radiobutton within w_gene_proceso_valorizacion
end type
type cbx_conserv from checkbox within w_gene_proceso_valorizacion
end type
type cbx_concate from checkbox within w_gene_proceso_valorizacion
end type
type cbx_convari from checkbox within w_gene_proceso_valorizacion
end type
type cbx_informe from checkbox within w_gene_proceso_valorizacion
end type
type cbx_valoriza from checkbox within w_gene_proceso_valorizacion
end type
type cbx_todosespe from checkbox within w_gene_proceso_valorizacion
end type
type cbx_todosprod from checkbox within w_gene_proceso_valorizacion
end type
type dw_especie from datawindow within w_gene_proceso_valorizacion
end type
type dw_productor from datawindow within w_gene_proceso_valorizacion
end type
type dw_exporta from datawindow within w_gene_proceso_valorizacion
end type
type st_10 from statictext within w_gene_proceso_valorizacion
end type
type em_fecha from editmask within w_gene_proceso_valorizacion
end type
type st_8 from statictext within w_gene_proceso_valorizacion
end type
type em_tipocambio from editmask within w_gene_proceso_valorizacion
end type
type st_7 from statictext within w_gene_proceso_valorizacion
end type
type st_6 from statictext within w_gene_proceso_valorizacion
end type
type dw_planta from datawindow within w_gene_proceso_valorizacion
end type
type sle_mensa from singlelineedit within w_gene_proceso_valorizacion
end type
type st_5 from statictext within w_gene_proceso_valorizacion
end type
type pb_salir from picturebutton within w_gene_proceso_valorizacion
end type
type pb_acepta from picturebutton within w_gene_proceso_valorizacion
end type
type st_2 from statictext within w_gene_proceso_valorizacion
end type
type st_1 from statictext within w_gene_proceso_valorizacion
end type
type st_titulo from statictext within w_gene_proceso_valorizacion
end type
type gb_2 from groupbox within w_gene_proceso_valorizacion
end type
type gb_1 from groupbox within w_gene_proceso_valorizacion
end type
type r_1 from rectangle within w_gene_proceso_valorizacion
end type
type gb_3 from groupbox within w_gene_proceso_valorizacion
end type
type gb_4 from groupbox within w_gene_proceso_valorizacion
end type
type gb_5 from groupbox within w_gene_proceso_valorizacion
end type
type st_3 from statictext within w_gene_proceso_valorizacion
end type
end forward

global type w_gene_proceso_valorizacion from window
integer x = 1074
integer y = 484
integer width = 2830
integer height = 2152
boolean titlebar = true
string title = "Generación de Procesos de Pre-Facturación"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 12632256
cbx_conplan cbx_conplan
cbx_conprod cbx_conprod
cbx_archivo cbx_archivo
dw_registro dw_registro
em_porceniva em_porceniva
st_4 st_4
cbx_todasplan cbx_todasplan
rb_ambos rb_ambos
rb_salida rb_salida
rb_ingreso rb_ingreso
cbx_conserv cbx_conserv
cbx_concate cbx_concate
cbx_convari cbx_convari
cbx_informe cbx_informe
cbx_valoriza cbx_valoriza
cbx_todosespe cbx_todosespe
cbx_todosprod cbx_todosprod
dw_especie dw_especie
dw_productor dw_productor
dw_exporta dw_exporta
st_10 st_10
em_fecha em_fecha
st_8 st_8
em_tipocambio em_tipocambio
st_7 st_7
st_6 st_6
dw_planta dw_planta
sle_mensa sle_mensa
st_5 st_5
pb_salir pb_salir
pb_acepta pb_acepta
st_2 st_2
st_1 st_1
st_titulo st_titulo
gb_2 gb_2
gb_1 gb_1
r_1 r_1
gb_3 gb_3
gb_4 gb_4
gb_5 gb_5
st_3 st_3
end type
global w_gene_proceso_valorizacion w_gene_proceso_valorizacion

type variables
str_busqueda	istr_busq
Str_info			lstr_info

DataWindowChild	idwc_Planta, idwc_exporta, idwc_productor, idwc_especie

uo_plantadesp			iuo_Planta
uo_especie				iuo_Especie
uo_productores    	iuo_productor


Integer    ii_TipoOrden	
end variables

forward prototypes
public subroutine habilitagrabar ()
public function boolean generaarchivo (date ad_fecha, integer ai_planta)
end prototypes

public subroutine habilitagrabar ();
end subroutine

public function boolean generaarchivo (date ad_fecha, integer ai_planta);Long				ll_filas, ll_fila, ll_filadet
String   		ls_registro, ls_archivo
DataStore      lds_doctosvalori, lds_valorizacion


lds_doctosvalori		=	Create DataStore
lds_valorizacion		=	Create DataStore

dw_Registro.SetTransObject(sqlca)

lds_doctosvalori.dataobject= "dw_mues_spro_controldoctosvalori_genera"
lds_doctosvalori.SetTransObject(sqlca)

lds_valorizacion.dataobject= "dw_mues_spro_controlvalorizacion_genera"
lds_valorizacion.SetTransObject(sqlca)


// Tabla Spro_Controldoctosvalori
ll_filas		= lds_doctosvalori.Retrieve(ad_fecha, ai_planta)

IF ll_filas = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura ")
	Return FALSE
	
ELSEIF ll_filas = 0 THEN
	MessageBox("Atención", "No hay información con Operación Indicada.~r~rIngrese otra Operación.", &
					Exclamation!, Ok!)
	Return FALSE
ELSEIF ll_filas > 0 THEN

	FOR ll_fila = 1 TO ll_filas
		ls_registro = '1'

		IF ISNULL(lds_doctosvalori.Object.cdva_fecpro[ll_fila]) THEN
			ls_Registro	+= Fill(' ',10)
		ELSE	
			ls_Registro	+=	String(lds_doctosvalori.Object.cdva_fecpro[ll_fila], 'dd/mm/yyyy')
		END IF		
		
		IF ISNULL(lds_doctosvalori.Object.cdva_tipdoc[ll_fila]) THEN
			ls_Registro	+= Fill(' ',1)
		ELSE	
			ls_Registro	+=	String(lds_doctosvalori.Object.cdva_tipdoc[ll_fila], '0')
		END IF		
		
		IF ISNULL(lds_doctosvalori.Object.plde_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(lds_doctosvalori.Object.plde_codigo[ll_fila], '0000')
		END IF		
		
		IF ISNULL(lds_doctosvalori.Object.prod_codigo[ll_fila]) THEN
			ls_Registro	+= Fill('', 4)
		ELSE	
			ls_Registro	+=	String(lds_doctosvalori.Object.prod_codigo[ll_fila], '00000')
		END IF		
			
		IF ISNULL(lds_doctosvalori.Object.cdva_guisii[ll_fila]) THEN
			ls_Registro	+= Fill(' ',8)
		ELSE	
			ls_Registro	+=	String(lds_doctosvalori.Object.cdva_guisii[ll_fila], '00000000')
		END IF		
		
		ll_filadet	= dw_Registro.InsertRow(0)		
		dw_Registro.object.registro[ll_filadet]=ls_Registro	
	NEXT
	
// Tabla Spro_controlvalorizacion	
ll_filas	=	lds_valorizacion.retrieve(ad_fecha, ai_planta)

	FOR ll_fila = 1 TO ll_filas
		ls_registro		= '2'
		
		IF ISNULL(lds_valorizacion.Object.cvap_fecpro[ll_fila]) THEN
			ls_Registro	+= Fill(' ',1)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.cvap_fecpro[ll_fila], 'dd/mm/yyyy')
		END IF		
		
		IF ISNULL(lds_valorizacion.Object.cvap_tipdoc[ll_fila]) THEN
			ls_Registro	+= Fill(' ',1)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.cvap_tipdoc[ll_fila], '0')
		END IF		
		
		IF ISNULL(lds_valorizacion.Object.plde_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ',4)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.plde_codigo[ll_fila], '0000')
		END IF		
		
		IF ISNULL(lds_valorizacion.Object.prod_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 4)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.prod_codigo[ll_fila], '00000')
		END IF		
		
		IF ISNULL(lds_valorizacion.Object.espe_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 2)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.espe_codigo[ll_fila], '00')
		END IF		
		
		IF ISNULL(lds_valorizacion.Object.vari_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 4)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.vari_codigo[ll_fila], '0000')
		END IF		
		
		IF ISNULL(lds_valorizacion.Object.enva_tipoen[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 1)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.enva_tipoen[ll_fila], '0')
		END IF		

		IF ISNULL(lds_valorizacion.Object.enva_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 3)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.enva_codigo[ll_fila], '000')
		END IF		

		IF ISNULL(lds_valorizacion.Object.cate_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 3)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.cate_codigo[ll_fila], '000')
		END IF		
		
 	   IF ISNULL(lds_valorizacion.Object.sepl_codigo[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 2)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.sepl_codigo[ll_fila], '00')
		END IF		

		IF ISNULL(lds_valorizacion.Object.cvap_totkil[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 10)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.cvap_totkil[ll_fila], '0000000000')
		END IF		
		
		IF ISNULL(lds_valorizacion.Object.cvap_totcaj[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 7)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.cvap_totcaj[ll_fila], '0000000')
		END IF		
		
		IF ISNULL(lds_valorizacion.Object.cvap_tipcam[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 6)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.cvap_tipcam[ll_fila], '000000')
		END IF		

		IF ISNULL(lds_valorizacion.Object.cvap_vaneus[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 7)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.cvap_vaneus[ll_fila], '0000000')
		END IF
		
		IF ISNULL(lds_valorizacion.Object.cvap_vanepe[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 9)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.cvap_vanepe[ll_fila], '000000000')
		END IF
		
		IF ISNULL(lds_valorizacion.Object.cvap_totnet[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 12)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.cvap_totnet[ll_fila], '000000000000')
		END IF		
		
		IF ISNULL(lds_valorizacion.Object.cvap_valiva[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 12)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.cvap_valiva[ll_fila], '000000000000')
		END IF
		
		IF ISNULL(lds_valorizacion.Object.cvap_valtot[ll_fila]) THEN
			ls_Registro	+= Fill(' ', 12)
		ELSE	
			ls_Registro	+=	String(lds_valorizacion.Object.cvap_valtot[ll_fila], '000000000000')
		END IF

		ll_filadet	=dw_Registro.insertrow(0)		
		dw_Registro.object.registro[ll_filadet]=ls_Registro	
		
	NEXT
END IF

IF dw_Registro.SaveAs("",Text!,False) = -1 THEN
	MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)
	Return FALSE
ELSE
	Return TRUE
END IF
end function

on w_gene_proceso_valorizacion.create
this.cbx_conplan=create cbx_conplan
this.cbx_conprod=create cbx_conprod
this.cbx_archivo=create cbx_archivo
this.dw_registro=create dw_registro
this.em_porceniva=create em_porceniva
this.st_4=create st_4
this.cbx_todasplan=create cbx_todasplan
this.rb_ambos=create rb_ambos
this.rb_salida=create rb_salida
this.rb_ingreso=create rb_ingreso
this.cbx_conserv=create cbx_conserv
this.cbx_concate=create cbx_concate
this.cbx_convari=create cbx_convari
this.cbx_informe=create cbx_informe
this.cbx_valoriza=create cbx_valoriza
this.cbx_todosespe=create cbx_todosespe
this.cbx_todosprod=create cbx_todosprod
this.dw_especie=create dw_especie
this.dw_productor=create dw_productor
this.dw_exporta=create dw_exporta
this.st_10=create st_10
this.em_fecha=create em_fecha
this.st_8=create st_8
this.em_tipocambio=create em_tipocambio
this.st_7=create st_7
this.st_6=create st_6
this.dw_planta=create dw_planta
this.sle_mensa=create sle_mensa
this.st_5=create st_5
this.pb_salir=create pb_salir
this.pb_acepta=create pb_acepta
this.st_2=create st_2
this.st_1=create st_1
this.st_titulo=create st_titulo
this.gb_2=create gb_2
this.gb_1=create gb_1
this.r_1=create r_1
this.gb_3=create gb_3
this.gb_4=create gb_4
this.gb_5=create gb_5
this.st_3=create st_3
this.Control[]={this.cbx_conplan,&
this.cbx_conprod,&
this.cbx_archivo,&
this.dw_registro,&
this.em_porceniva,&
this.st_4,&
this.cbx_todasplan,&
this.rb_ambos,&
this.rb_salida,&
this.rb_ingreso,&
this.cbx_conserv,&
this.cbx_concate,&
this.cbx_convari,&
this.cbx_informe,&
this.cbx_valoriza,&
this.cbx_todosespe,&
this.cbx_todosprod,&
this.dw_especie,&
this.dw_productor,&
this.dw_exporta,&
this.st_10,&
this.em_fecha,&
this.st_8,&
this.em_tipocambio,&
this.st_7,&
this.st_6,&
this.dw_planta,&
this.sle_mensa,&
this.st_5,&
this.pb_salir,&
this.pb_acepta,&
this.st_2,&
this.st_1,&
this.st_titulo,&
this.gb_2,&
this.gb_1,&
this.r_1,&
this.gb_3,&
this.gb_4,&
this.gb_5,&
this.st_3}
end on

on w_gene_proceso_valorizacion.destroy
destroy(this.cbx_conplan)
destroy(this.cbx_conprod)
destroy(this.cbx_archivo)
destroy(this.dw_registro)
destroy(this.em_porceniva)
destroy(this.st_4)
destroy(this.cbx_todasplan)
destroy(this.rb_ambos)
destroy(this.rb_salida)
destroy(this.rb_ingreso)
destroy(this.cbx_conserv)
destroy(this.cbx_concate)
destroy(this.cbx_convari)
destroy(this.cbx_informe)
destroy(this.cbx_valoriza)
destroy(this.cbx_todosespe)
destroy(this.cbx_todosprod)
destroy(this.dw_especie)
destroy(this.dw_productor)
destroy(this.dw_exporta)
destroy(this.st_10)
destroy(this.em_fecha)
destroy(this.st_8)
destroy(this.em_tipocambio)
destroy(this.st_7)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.sle_mensa)
destroy(this.st_5)
destroy(this.pb_salir)
destroy(this.pb_acepta)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.st_titulo)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.r_1)
destroy(this.gb_3)
destroy(this.gb_4)
destroy(this.gb_5)
destroy(this.st_3)
end on

event open;X	=	0
Y	=	0
//Planta
dw_planta.GetChild("plde_codigo",idwc_planta)
idwc_planta.SetTransObject(SQLCA)

IF idwc_planta.Retrieve(gi_codexport)=0 THEN
	idwc_planta.InsertRow(0)
END IF

dw_planta.SetTransObject(SQLCA)
dw_planta.InsertRow(0)
dw_planta.SetItem(1, "plde_codigo", gstr_paramplanta.codigoplanta)
dw_planta.Enabled = False
dw_planta.Object.plde_codigo.BackGround.Color = RGB(192,192,192)

//Exportador
dw_exporta.GetChild("expo_codigo",idwc_exporta)
idwc_exporta.SetTransObject(SQLCA)

IF idwc_exporta.Retrieve()=0 THEN
	idwc_exporta.InsertRow(0)
END IF

dw_exporta.SetTransObject(SQLCA)
dw_exporta.InsertRow(0)
dw_exporta.SetItem(1, "expo_codigo", gi_codexport)
dw_exporta.Enabled = False
dw_exporta.Object.expo_codigo.BackGround.Color = RGB(192,192,192)

//Productor
dw_productor.GetChild("prod_codigo",idwc_productor)
idwc_productor.SetTransObject(SQLCA)

IF idwc_productor.Retrieve()=0 THEN
	idwc_productor.InsertRow(0)
END IF

dw_productor.SetTransObject(SQLCA)
dw_productor.InsertRow(0)
dw_productor.Enabled = False
dw_productor.Object.prod_codigo.BackGround.Color = RGB(192,192,192)

//Especie
dw_especie.GetChild("espe_codigo",idwc_especie)
idwc_especie.SetTransObject(SQLCA)

IF idwc_especie.Retrieve(gstr_parempresa.empr_codexp)=0 THEN
	idwc_especie.InsertRow(0)
END IF

dw_especie.SetTransObject(SQLCA)
dw_especie.InsertRow(0)
dw_especie.Enabled = False
dw_especie.Object.espe_codigo.BackGround.Color = RGB(192,192,192)

em_fecha.text	   =	String(Date(Today()),'mm/yyyy')
em_porceniva.text =  String(gstr_paramplanta.porcentajeiva,"00,00")

iuo_Especie				=	CREATE uo_Especie
iuo_Productor  		=  CREATE uo_Productores


end event

type cbx_conplan from checkbox within w_gene_proceso_valorizacion
integer x = 1463
integer y = 1452
integer width = 361
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Planta"
end type

type cbx_conprod from checkbox within w_gene_proceso_valorizacion
integer x = 1842
integer y = 1452
integer width = 402
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Productor"
end type

type cbx_archivo from checkbox within w_gene_proceso_valorizacion
integer x = 850
integer y = 1252
integer width = 489
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Archivo Plano"
end type

type dw_registro from datawindow within w_gene_proceso_valorizacion
integer x = 965
integer y = 2568
integer width = 603
integer height = 100
integer taborder = 100
string title = "none"
string dataobject = "dw_registro_traspaso"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type em_porceniva from editmask within w_gene_proceso_valorizacion
integer x = 617
integer y = 1032
integer width = 384
integer height = 92
integer taborder = 90
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 1090519039
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "##.00"
end type

type st_4 from statictext within w_gene_proceso_valorizacion
integer x = 165
integer y = 1040
integer width = 402
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "% I.V.A."
boolean focusrectangle = false
end type

type cbx_todasplan from checkbox within w_gene_proceso_valorizacion
integer x = 1541
integer y = 404
integer width = 288
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;Integer li_Null

SetNull(li_Null)

IF cbx_todasplan.checked THEN
	
	dw_planta.Setitem(1,"plde_codigo",li_Null)
	dw_planta.Enabled = FALSE
	dw_planta.Object.plde_codigo.BackGround.Color = RGB(192,192,192)
	
ELSE
	
	dw_planta.Enabled = True
	dw_planta.Object.plde_codigo.BackGround.Color = RGB(255,255,255)
	dw_planta.SetFocus()
	
END IF	
end event

type rb_ambos from radiobutton within w_gene_proceso_valorizacion
integer x = 1559
integer y = 1640
integer width = 402
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Ambos"
end type

type rb_salida from radiobutton within w_gene_proceso_valorizacion
integer x = 1029
integer y = 1644
integer width = 402
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Salida"
end type

type rb_ingreso from radiobutton within w_gene_proceso_valorizacion
integer x = 485
integer y = 1644
integer width = 402
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Ingresos"
boolean checked = true
end type

type cbx_conserv from checkbox within w_gene_proceso_valorizacion
integer x = 1056
integer y = 1452
integer width = 393
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Servicios"
end type

type cbx_concate from checkbox within w_gene_proceso_valorizacion
integer x = 635
integer y = 1452
integer width = 407
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Categoria"
end type

type cbx_convari from checkbox within w_gene_proceso_valorizacion
integer x = 229
integer y = 1452
integer width = 402
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Variedad"
end type

type cbx_informe from checkbox within w_gene_proceso_valorizacion
integer x = 1554
integer y = 1252
integer width = 699
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "  Informe de Revisión"
end type

event clicked;IF cbx_informe.Checked THEN
	
	cbx_ConVari.Enabled  =	True
	cbx_ConCate.Enabled	=	True
	cbx_ConServ.Enabled	=	True
	cbx_ConPlan.Enabled	=	True
	cbx_ConProd.Enabled	=	True

	Rb_Ingreso.Enabled	=	True
	Rb_Salida.Enabled		=	True
	Rb_Ambos.Enabled		=	True

ELSE
	
	cbx_ConVari.Enabled  =	False
	cbx_ConCate.Enabled	=	False
	cbx_ConServ.Enabled	=	False
	cbx_ConPlan.Enabled	=	False
	cbx_ConProd.Enabled	=	False
	
	Rb_Ingreso.Enabled	=	False
	Rb_Salida.Enabled		=	False
	Rb_Ambos.Enabled		=	False
	
END IF
end event

type cbx_valoriza from checkbox within w_gene_proceso_valorizacion
integer x = 256
integer y = 1252
integer width = 535
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Valoriza"
end type

event clicked;IF cbx_valoriza.Checked THEN
	cbx_Archivo.Enabled = TRUE
ELSE
	cbx_Archivo.Enabled = False
	cbx_Archivo.Checked = False
END IF
end event

type cbx_todosespe from checkbox within w_gene_proceso_valorizacion
integer x = 1541
integer y = 648
integer width = 297
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todas"
boolean checked = true
end type

event clicked;Integer li_Null

SetNull(li_Null)

IF cbx_todosespe.checked THEN
	
	dw_especie.Setitem(1,"espe_codigo",li_Null)
	dw_especie.Enabled = FALSE
	dw_especie.Object.espe_codigo.BackGround.Color = RGB(192,192,192)
	
ELSE
	
	dw_especie.Enabled = True
	dw_especie.Object.espe_codigo.BackGround.Color = RGB(255,255,255)
	dw_especie.SetFocus()
	
END IF	
end event

type cbx_todosprod from checkbox within w_gene_proceso_valorizacion
integer x = 1541
integer y = 760
integer width = 288
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Todos"
boolean checked = true
end type

event clicked;Integer li_Null

SetNull(li_Null)

IF cbx_todosprod.checked THEN
	
	dw_productor.Setitem(1,"prod_codigo",li_Null)
	dw_productor.Enabled = FALSE
	dw_productor.Object.prod_codigo.BackGround.Color = RGB(192,192,192)
	
ELSE
	
	dw_productor.Enabled = True
	dw_productor.Object.prod_codigo.BackGround.Color = RGB(255,255,255)
	dw_productor.SetFocus()
	
END IF	
end event

type dw_especie from datawindow within w_gene_proceso_valorizacion
integer x = 617
integer y = 640
integer width = 923
integer height = 96
integer taborder = 70
boolean enabled = false
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

type dw_productor from datawindow within w_gene_proceso_valorizacion
integer x = 617
integer y = 764
integer width = 887
integer height = 108
integer taborder = 30
boolean enabled = false
string title = "none"
string dataobject = "dddw_productores"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_exporta from datawindow within w_gene_proceso_valorizacion
integer x = 617
integer y = 280
integer width = 1102
integer height = 112
integer taborder = 10
string title = "none"
string dataobject = "dddw_exportadores"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_10 from statictext within w_gene_proceso_valorizacion
integer x = 165
integer y = 284
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Exportador"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_gene_proceso_valorizacion
integer x = 617
integer y = 520
integer width = 389
integer height = 92
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 1090519039
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "mm/yyyy"
end type

type st_8 from statictext within w_gene_proceso_valorizacion
integer x = 165
integer y = 536
integer width = 457
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Mes a Procesar"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_tipocambio from editmask within w_gene_proceso_valorizacion
integer x = 617
integer y = 900
integer width = 384
integer height = 92
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 1090519039
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "#,###.##"
end type

type st_7 from statictext within w_gene_proceso_valorizacion
integer x = 165
integer y = 912
integer width = 411
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Tipo Cambio"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_6 from statictext within w_gene_proceso_valorizacion
integer x = 165
integer y = 420
integer width = 370
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_gene_proceso_valorizacion
integer x = 617
integer y = 400
integer width = 882
integer height = 92
boolean enabled = false
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;IF data <> '' THEN
	IF idwc_Planta.Find("plde_codigo = " + data, 1, idwc_Planta.RowCount()) = 0 THEN
		MessageBox("Atención", "Código de Planta indicado no ha sido~r" + &
						"creado en tabla respectiva.~r~rIngrese o seleccione" + &
						"otra Planta.")
		
		RETURN 1
	END IF
END IF
end event

type sle_mensa from singlelineedit within w_gene_proceso_valorizacion
integer x = 133
integer y = 1832
integer width = 2167
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_gene_proceso_valorizacion
integer x = 69
integer y = 1768
integer width = 2290
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_gene_proceso_valorizacion
integer x = 2519
integer y = 928
integer width = 155
integer height = 132
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\desarrollo\bmp\exit.bmp"
alignment htextalign = right!
end type

event clicked;Close(Parent)
end event

type pb_acepta from picturebutton within w_gene_proceso_valorizacion
event clicked pbm_bnclicked
integer x = 2519
integer y = 604
integer width = 155
integer height = 132
integer taborder = 50
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\desarrollo\bmp\disksave.bmp"
string disabledname = "\desarrollo\bmp\disksavd.bmp"
alignment htextalign = right!
end type

event clicked;Integer	li_Planta, li_Especie, li_Exporta, li_mes, li_ano, &
         li_ConVariedad, li_ConCategoria, li_ConServicio, li_TipoInforme
Long     ll_fila, ll_Productor
String   ls_fecha
Date		ld_Hasta, ld_Desde, ld_fecha
Decimal{2} ld_tipocambio, ld_porceniva

SetPointer(HourGlass!)

sle_mensa.text	=	""

li_Planta		=	dw_Planta.Object.plde_codigo[1]
IF IsNull(li_Planta) THEN
	MessageBox("Atención","Debe Seleccionar Planta",Exclamation!)
	
	RETURN
END IF

li_Exporta		=	dw_Exporta.Object.expo_codigo[1]
IF IsNull(li_Exporta) THEN
	MessageBox("Atención","Debe Seleccionar un Exportador",Exclamation!)
	RETURN
END IF

ls_fecha = '01/' + em_fecha.text
li_mes   =  integer(mid(ls_fecha,4,2)) + 1
li_ano   =  integer(mid(ls_fecha,7,4))

IF IsNull(li_mes) or li_mes = 1 or li_mes = 0 THEN
	MessageBox("Atención","Debe Seleccionar un Mes/Año de Proceso",Exclamation!)
	RETURN
END IF

IF IsNull(li_ano) or li_ano < 1930 or li_ano = 0  or li_ano > 2050 THEN
	MessageBox("Atención","Debe Seleccionar un Mes/Año de Proceso Valido",Exclamation!)
	RETURN
END IF

IF li_mes = 13 THEN
	li_mes = 1
   li_ano = li_ano + 1
END IF	


ld_fecha = Date(ls_fecha)

IF cbx_todosprod.Checked THEN
	ll_productor = 0
ELSE
	ll_Productor		=	dw_productor.Object.prod_codigo[1]
	IF IsNull(ll_Productor) THEN
		MessageBox("Atención","Debe Seleccionar un Productor",Exclamation!)
		RETURN
	END IF	
END IF


IF cbx_todosespe.Checked THEN
	li_especie = 0
ELSE
	li_especie		=	dw_especie.Object.espe_codigo[1]
	IF IsNull(li_especie) THEN
		MessageBox("Atención","Debe Seleccionar una especie",Exclamation!)
		RETURN
	END IF	
END IF

ld_tipocambio		=	dec(em_tipocambio.text)

IF IsNull(ld_tipocambio) OR ld_tipocambio <= 0 THEN
	MessageBox("Atención","Debe Ingresar un Tipo Cambio Mayor a Cero",Exclamation!)
	RETURN
END IF	

ld_porceniva		=	dec(em_porceniva.text)


IF IsNull(ld_porceniva) OR ld_Porceniva > 100 OR ld_Porceniva < 0 THEN
	MessageBox("Atención","Debe Ingresar un Porcentaje de I.V.A. entre 1 y 100)",Exclamation!)
	RETURN
END IF	


IF cbx_valoriza.checked THEN
   
	sle_mensa.text	=	"Generando Información"
	
	DECLARE	Valorizacion PROCEDURE FOR dba.FGran_ProcesoValorizacion
	         @Exportador    =:  li_Exporta,
				@Planta 			=:	 li_Planta,				
				@mespro        =:  ld_fecha,
				@Especie			=:  li_Especie,
				@Productor     =:  ll_Productor,				
				@TipoCambio    =:  ld_Tipocambio,
				@PorcenIva		=:  ld_Porceniva
				
	USING SQLCA ;
			
	EXECUTE Valorizacion;	
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura del Procedimiento Almacenado " + &
								"FGran_ProcesoValorizacion" )
		sle_mensa.text	=	"Error al Generar Datos."
	ELSEIF cbx_Archivo.Checked THEN
		
			 IF GeneraArchivo(Date(ld_fecha), li_Planta) THEN
				 sle_Mensa.text	= "Archivo Generado. Avise a Computación"		
			  ELSE			
				 sle_Mensa.text	= "El Archivo no se Pudo Generar"					
			  END IF
	END IF	
	sle_mensa.text	=	"Información Generada"		
	CLOSE Valorizacion;

ELSE
	sle_Mensa.Text = "Valorización no Generada"
END IF

IF cbx_informe.Checked THEN

   sle_mensa.text	=	"Generando Informe"

	IF cbx_convari.Checked THEN
	   li_ConVariedad = -1
	ELSE
		li_ConVariedad = 0
	END IF	
	
	IF cbx_concate.Checked THEN
	   li_ConCategoria = -1
	ELSE
		li_ConCategoria = 0
	END IF
	
	IF cbx_conserv.Checked THEN
	   li_ConServicio = -1
	ELSE
		li_ConServicio = 0
	END IF
	
	IF cbx_conprod.Checked THEN
	   li_Planta = -1
	END IF

	IF cbx_conplan.Checked THEN
	   ll_Productor = -1
	END IF

	IF rb_ingreso.Checked THEN li_TipoInforme = 1
	IF rb_salida.Checked THEN li_TipoInforme  = 2
	IF rb_ambos.Checked THEN li_TipoInforme   = 0
	
		
	lstr_info.titulo	= 'INFORME DETALLE DE FACTURAS PROVISORIAS'

	OpenWithParm(vinf, lstr_info)

	vinf.dw_1.DataObject = "dw_info_control_valorizacion"
	
	vinf.dw_1.SetTransObject(sqlca)

	ll_Fila	=	vinf.dw_1.Retrieve(li_Exporta,li_planta,ld_Fecha,ll_Productor,li_Especie, &
                          li_ConVariedad, li_ConCategoria, li_ConServicio, li_TipoInforme)

	IF ll_Fila = -1 THEN
		MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
      sle_mensa.text	=	"Error en Base de Datos."
	ELSEIF ll_Fila = 0 THEN
		MessageBox( "No Existe información", "No existe información para este informe.", &
	   	          StopSign!, Ok!)
      sle_mensa.text	=	"Debe generar la Valorización para generar informe"
	ELSE
	   F_Membrete(vinf.dw_1)
		vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
		vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
		vinf.Visible	= True
		vinf.Enabled	= True
		sle_mensa.text	=	"Proceso Terminado Satisfactoriamente."
	END IF
	
END IF	

SetPointer(Arrow!)
end event

type st_2 from statictext within w_gene_proceso_valorizacion
integer x = 165
integer y = 780
integer width = 384
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type st_1 from statictext within w_gene_proceso_valorizacion
integer x = 165
integer y = 652
integer width = 370
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_titulo from statictext within w_gene_proceso_valorizacion
integer x = 82
integer y = 68
integer width = 2290
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Generación de Pre - Valorización"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type gb_2 from groupbox within w_gene_proceso_valorizacion
integer x = 2459
integer y = 840
integer width = 274
integer height = 272
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type gb_1 from groupbox within w_gene_proceso_valorizacion
integer x = 2459
integer y = 520
integer width = 274
integer height = 272
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type r_1 from rectangle within w_gene_proceso_valorizacion
long linecolor = 12632256
integer linethickness = 4
integer x = 773
integer y = 588
integer width = 165
integer height = 144
end type

type gb_3 from groupbox within w_gene_proceso_valorizacion
integer x = 146
integer y = 1376
integer width = 2158
integer height = 188
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " Consolida por "
end type

type gb_4 from groupbox within w_gene_proceso_valorizacion
integer x = 146
integer y = 1568
integer width = 2158
integer height = 188
integer taborder = 90
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " Tipo de Informe "
end type

type gb_5 from groupbox within w_gene_proceso_valorizacion
integer x = 146
integer y = 1184
integer width = 2158
integer height = 188
integer taborder = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = " Genera "
end type

type st_3 from statictext within w_gene_proceso_valorizacion
integer x = 73
integer y = 236
integer width = 2290
integer height = 1532
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

